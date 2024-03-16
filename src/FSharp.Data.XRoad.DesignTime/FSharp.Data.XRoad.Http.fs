module internal FSharp.Data.XRoad.Http

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Xml
open System.Xml.Linq

let [<Literal>] XML_CONTENT_TYPE = "text/xml; charset=UTF-8"

let utf8WithoutBom = UTF8Encoding(false)

[<RequireQualifiedAccess>]
module MediaTypeNames =
    module Application =
        let Xml =
#if NETSTANDARD2_1_OR_GREATER
            System.Net.Mime.MediaTypeNames.Application.Xml
#else
            "application/xml"
#endif
    module Text =
        let Xml =
            System.Net.Mime.MediaTypeNames.Text.Xml

let downloadFile (fileInfo: FileInfo) (path: string) (httpClient: HttpClient) =
    async {
        use request = new HttpRequestMessage(HttpMethod.Get, path)
        request.Headers.Accept.Add(MediaTypeWithQualityHeaderValue(MediaTypeNames.Application.Xml))
        use! response = httpClient.SendAsync(request) |> Async.AwaitTask
        response.EnsureSuccessStatusCode() |> ignore
        use! responseStream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
        use file = fileInfo.OpenWrite()
        file.SetLength(0L)
        responseStream.CopyTo(file)
    }

let post (stream: MemoryStream) (path: string) (httpClient: HttpClient) =
    async {
        use request = new HttpRequestMessage(HttpMethod.Post, path)
        request.Headers.Accept.Add(MediaTypeWithQualityHeaderValue(MediaTypeNames.Application.Xml))
        request.Headers.TryAddWithoutValidation("SOAPAction", "") |> ignore
        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        use content = new StreamContent(stream)
        content.Headers.ContentType <- MediaTypeHeaderValue(MediaTypeNames.Text.Xml)
        use! response = httpClient.SendAsync(request) |> Async.AwaitTask
        response.EnsureSuccessStatusCode() |> ignore
        use! contentStream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
        return XDocument.Load(contentStream)
    }

/// Remember previously downloaded content in temporary files.
let cache = ConcurrentDictionary<Uri, FileInfo>()

/// Downloads producer list if not already downloaded previously.
/// Can be forced to redownload file by `refresh` parameters.
let getFile skipCache (path: string) (httpClient: HttpClient) =
    let f (_: Uri) =
        let file = FileInfo(Path.GetTempFileName())
        httpClient |> downloadFile file path |> Async.RunSynchronously
        file
    let uri = Uri(httpClient.BaseAddress, path)
    let file = if skipCache then cache.AddOrUpdate(uri, f, (fun uri _ -> f uri)) else cache.GetOrAdd(uri, f)
    use stream = file.OpenRead()
    XDocument.Load(stream)

let buildRequest (writer: XmlWriter) (writeBody: unit -> unit) (client: XRoadMemberIdentifier) (service: XRoadServiceIdentifier) =
    writer.WriteStartDocument()
    writer.WriteStartElement("soapenv", "Envelope", XmlNamespace.SoapEnv)
    writer.WriteAttributeString("xmlns", "soapenv", XmlNamespace.Xmlns, XmlNamespace.SoapEnv)
    writer.WriteAttributeString("xmlns", "xro", XmlNamespace.Xmlns, XmlNamespace.XRoad)
    writer.WriteAttributeString("xmlns", "iden", XmlNamespace.Xmlns, XmlNamespace.XRoadIdentifiers)
    writer.WriteStartElement("Header", XmlNamespace.SoapEnv)
    writer.WriteElementString("protocolVersion", XmlNamespace.XRoad, "4.0")
    writer.WriteElementString("id", XmlNamespace.XRoad, getUUID())
    writer.WriteStartElement("service", XmlNamespace.XRoad)
    writer.WriteAttributeString("objectType", XmlNamespace.XRoadIdentifiers, service.ObjectId)
    writer.WriteElementString("xRoadInstance", XmlNamespace.XRoadIdentifiers, service.Owner.XRoadInstance)
    writer.WriteElementString("memberClass", XmlNamespace.XRoadIdentifiers, service.Owner.MemberClass)
    writer.WriteElementString("memberCode", XmlNamespace.XRoadIdentifiers, service.Owner.MemberCode)
    service.Owner.SubsystemCode |> strToOption |> Option.iter (fun code -> writer.WriteElementString("subsystemCode", XmlNamespace.XRoadIdentifiers, code))
    writer.WriteElementString("serviceCode", XmlNamespace.XRoadIdentifiers, service.ServiceCode)
    service.ServiceVersion |> strToOption |> Option.iter (fun version -> writer.WriteElementString("serviceVersion", XmlNamespace.XRoadIdentifiers, version))
    writer.WriteEndElement()
    writer.WriteStartElement("client", XmlNamespace.XRoad)
    writer.WriteAttributeString("objectType", XmlNamespace.XRoadIdentifiers, client.ObjectId)
    writer.WriteElementString("xRoadInstance", XmlNamespace.XRoadIdentifiers, client.XRoadInstance)
    writer.WriteElementString("memberClass", XmlNamespace.XRoadIdentifiers, client.MemberClass)
    writer.WriteElementString("memberCode", XmlNamespace.XRoadIdentifiers, client.MemberCode)
    client.SubsystemCode |> strToOption |> Option.iter (fun code -> writer.WriteElementString("subsystemCode", XmlNamespace.XRoadIdentifiers, code))
    writer.WriteEndElement()
    writer.WriteEndElement()
    writer.WriteStartElement("Body", XmlNamespace.SoapEnv)
    writeBody()
    writer.WriteEndElement()
    writer.WriteEndElement()
    writer.WriteEndDocument()
    writer.Flush()

type XRoadMember = { Code: string; Name: string; Subsystems: string list }
type XRoadMemberClass = { Name: string; Members: XRoadMember list }

/// Downloads and parses producer list for X-Road v6 security server.
let downloadProducerList instance skipCache (httpClient: HttpClient) =
    // Read xml document from file and navigate to root element.
    let doc = httpClient |> getFile skipCache $"listClients?xRoadInstance=%s{instance}"

    doc.Element(XName.Get("Envelope", XmlNamespace.SoapEnv))
    |> Option.ofObj
    |> Option.bind (fun x -> x.Element(XName.Get("Body", XmlNamespace.SoapEnv)) |> Option.ofObj)
    |> Option.bind (fun x -> x.Element(XName.Get("Fault", XmlNamespace.SoapEnv)) |> Option.ofObj)
    |> Option.map (fun x -> x.Element(XName.Get("faultstring")) |> Option.ofObj |> Option.map _.Value |> Option.defaultValue "Could not download producer list from security server")
    |> Option.iter failwith

    let root = doc.Element(XName.Get("clientList", XmlNamespace.XRoad))
    // Data structures to support recomposition to records.
    let subsystems = Dictionary<string * string, ISet<string>>()
    let members = Dictionary<string, ISet<string * string>>()
    // Collect data about members and subsystems.
    root.Elements(XName.Get("member", XmlNamespace.XRoad))
    |> Seq.iter (fun element ->
        let id = element.Element(XName.Get("id", XmlNamespace.XRoad))
        let memberClass = id.Element(XName.Get("memberClass", XmlNamespace.XRoadIdentifiers)).Value
        let memberCode = id.Element(XName.Get("memberCode", XmlNamespace.XRoadIdentifiers)).Value
        match id.Attribute(XName.Get("objectType", XmlNamespace.XRoadIdentifiers)).Value with
        | "MEMBER" ->
            let name = element.Element(XName.Get("name", XmlNamespace.XRoad)).Value
            match members.TryGetValue(memberClass) with
            | true, lst -> lst.Add(name, memberCode) |> ignore
            | false, _ -> members.Add(memberClass, SortedSet<_>([name, memberCode]))
        | "SUBSYSTEM" ->
            let subsystemCode = id.Element(XName.Get("subsystemCode", XmlNamespace.XRoadIdentifiers)).Value
            match subsystems.TryGetValue((memberClass, memberCode)) with
            | true, lst -> lst.Add(subsystemCode) |> ignore
            | false, _ -> subsystems.Add((memberClass, memberCode), SortedSet<_>([subsystemCode]))
        | x -> failwith $"Unexpected object type value `%s{x}`.")
    // Compose records from previously collected data.
    members
    |> Seq.map (fun kvp ->
        { Name = kvp.Key
          Members = kvp.Value
                    |> Seq.map (fun (name,code) ->
                        { Code = code
                          Name = name
                          Subsystems =
                            match subsystems.TryGetValue((kvp.Key, code)) with
                            | true, lst -> lst |> Seq.toList
                            | false, _ -> [] })
                    |> Seq.toList })
    |> Seq.sortBy _.Name
    |> Seq.toList


/// Downloads and parses central service list from X-Road v6 security server.
let downloadCentralServiceList instance skipCache (httpClient: HttpClient) =
    // Read xml document from file and navigate to root element.
    let doc = httpClient |> getFile skipCache $"listCentralServices?xRoadInstance=%s{instance}"
    let root = doc.Element(XName.Get("centralServiceList", XmlNamespace.XRoad))
    // Collect data about available central services.
    root.Elements(XName.Get("centralService", XmlNamespace.XRoad))
    |> Seq.map _.Element(XName.Get("serviceCode", XmlNamespace.XRoadIdentifiers)).Value
    |> Seq.sortBy id
    |> Seq.toList

/// Downloads and parses method list of selected service provider.
let downloadMethodsList (client: XRoadMemberIdentifier) (service: XRoadServiceIdentifier) httpClient =
    let doc =
        use stream = new MemoryStream()
        use streamWriter = new StreamWriter(stream, utf8WithoutBom)
        use writer = XmlWriter.Create(streamWriter)
        (client, service) ||> buildRequest writer (fun _ -> writer.WriteElementString("listMethods", XmlNamespace.XRoad))
        httpClient |> post stream "" |> Async.RunSynchronously
    let envelope = doc.Element(XName.Get("Envelope", XmlNamespace.SoapEnv))
    let body = envelope.Element(XName.Get("Body", XmlNamespace.SoapEnv))
    let fault = body.Element(XName.Get("Fault", XmlNamespace.SoapEnv))
    if not (isNull fault) then
        let code = fault.Element(XName.Get("faultcode")) |> Option.ofObj |> Option.fold (fun _ x -> x.Value) ""
        let text = fault.Element(XName.Get("faultstring")) |> Option.ofObj |> Option.fold (fun _ x -> x.Value) ""
        failwith $"Opration resulted with error: FaultCode: %s{code}; FaultString: %s{text}"
    body.Element(XName.Get("listMethodsResponse", XmlNamespace.XRoad)).Elements(XName.Get("service", XmlNamespace.XRoad))
    |> Seq.map (fun service ->
        XRoadServiceIdentifier(
            XRoadMemberIdentifier(
                service.Element(XName.Get("xRoadInstance", XmlNamespace.XRoadIdentifiers)).Value,
                service.Element(XName.Get("memberClass", XmlNamespace.XRoadIdentifiers)).Value,
                service.Element(XName.Get("memberCode", XmlNamespace.XRoadIdentifiers)).Value,
                service.Element(XName.Get("subsystemCode", XmlNamespace.XRoadIdentifiers)) |> Option.ofObj |> Option.map _.Value |> Option.defaultValue ""
            ),
            service.Element(XName.Get("serviceCode", XmlNamespace.XRoadIdentifiers)).Value,
            service.Element(XName.Get("serviceVersion", XmlNamespace.XRoadIdentifiers)) |> Option.ofObj |> Option.map _.Value |> Option.defaultValue ""
        )
    )
    |> Seq.toList

let getXDocument (uri: Uri) (httpClient: HttpClient) =
    match uri.Scheme.ToLower() with
    | "http" | "https" -> httpClient |> getFile false (uri.ToString())
    | _ -> XDocument.Load(uri.ToString())

/// Check if given uri is valid network location or file path in local file system.
let resolveUri uri =
    match Uri.IsWellFormedUriString(uri, UriKind.Absolute) with
    | true -> Uri(uri, UriKind.Absolute)
    | _ ->
        let fullPath = FileInfo(uri).FullName
        match File.Exists(fullPath) with
        | true -> Uri(fullPath)
        | _ -> failwith $"Cannot resolve url location `%s{uri}`"
