module internal FSharp.Data.XRoad.Http

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Net
open System.Text
open System.Xml
open System.Xml.Linq

let [<Literal>] XML_CONTENT_TYPE = "text/xml; charset=UTF-8"

let utf8WithoutBom = UTF8Encoding(false)

let createRequest (uri: Uri)  =
    ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls
    let request = WebRequest.Create(uri: Uri) |> unbox<HttpWebRequest>
    request.Accept <- "application/xml"
    if uri.Scheme = "https" then request.ServerCertificateValidationCallback <- (fun _ _ _ _ -> true)
    request

let downloadFile path uri =
    let request = uri |> createRequest
    use response = request.GetResponse()
    use responseStream = response.GetResponseStream()
    use file = File.OpenWrite(path)
    file.SetLength(0L)
    responseStream.CopyTo(file)

let post (stream: Stream) uri =
    let request = uri |> createRequest
    request.Method <- "POST"
    request.ContentType <- XML_CONTENT_TYPE
    request.Headers.Set("SOAPAction", "")
    use requestStream = request.GetRequestStream()
    stream.Position <- 0L
    stream.CopyTo(requestStream)
    requestStream.Flush()
    requestStream.Close()
    use response = request.GetResponse()
    use stream  = response.GetResponseStream()
    use contentStream = (stream, response) ||> MultipartMessage.read |> fst
    contentStream.Position <- 0L
    XDocument.Load(contentStream)

/// Remember previously downloaded content in temporary files.
let cache = ConcurrentDictionary<Uri, FileInfo>()

/// Downloads producer list if not already downloaded previously.
/// Can be forced to redownload file by `refresh` parameters.
let getFile refresh uri =
    let f uri =
        let fileName = Path.GetTempFileName()
        uri |> downloadFile fileName
        FileInfo(fileName)
    let file = if not refresh then cache.GetOrAdd(uri, f) else cache.AddOrUpdate(uri, f, (fun uri _ -> f uri))
    XDocument.Load(file.OpenRead())

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
let downloadProducerList uri instance refresh =
    // Read xml document from file and navigate to root element.
    let doc = Uri(uri, sprintf "listClients?xRoadInstance=%s" instance) |> getFile refresh

    doc.Element(XName.Get("Envelope", XmlNamespace.SoapEnv))
    |> Option.ofObj
    |> Option.bind (fun x -> x.Element(XName.Get("Body", XmlNamespace.SoapEnv)) |> Option.ofObj)
    |> Option.bind (fun x -> x.Element(XName.Get("Fault", XmlNamespace.SoapEnv)) |> Option.ofObj)
    |> Option.map (fun x -> x.Element(XName.Get("faultstring")) |> Option.ofObj |> Option.map (fun u -> u.Value) |> Option.defaultValue "Could not download producer list from security server")
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
        | x -> failwithf "Unexpected object type value `%s`." x)
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
    |> Seq.sortBy (fun x -> x.Name)
    |> Seq.toList


/// Downloads and parses central service list from X-Road v6 security server.
let downloadCentralServiceList uri instance refresh =
    // Read xml document from file and navigate to root element.
    let doc = Uri(uri, sprintf "listCentralServices?xRoadInstance=%s" instance) |> getFile refresh
    let root = doc.Element(XName.Get("centralServiceList", XmlNamespace.XRoad))
    // Collect data about available central services.
    root.Elements(XName.Get("centralService", XmlNamespace.XRoad))
    |> Seq.map (fun element -> element.Element(XName.Get("serviceCode", XmlNamespace.XRoadIdentifiers)).Value)
    |> Seq.sortBy (id)
    |> Seq.toList

/// Downloads and parses method list of selected service provider.
let downloadMethodsList uri (client: XRoadMemberIdentifier) (service: XRoadServiceIdentifier) =
    let doc =
        use stream = new MemoryStream()
        use streamWriter = new StreamWriter(stream, utf8WithoutBom)
        use writer = XmlWriter.Create(streamWriter)
        (client, service) ||> buildRequest writer (fun _ -> writer.WriteElementString("listMethods", XmlNamespace.XRoad))
        post stream uri
    let envelope = doc.Element(XName.Get("Envelope", XmlNamespace.SoapEnv))
    let body = envelope.Element(XName.Get("Body", XmlNamespace.SoapEnv))
    let fault = body.Element(XName.Get("Fault", XmlNamespace.SoapEnv))
    if not (isNull fault) then
        let code = fault.Element(XName.Get("faultcode")) |> Option.ofObj |> Option.fold (fun _ x -> x.Value) ""
        let text = fault.Element(XName.Get("faultstring")) |> Option.ofObj |> Option.fold (fun _ x -> x.Value) ""
        failwithf "Opration resulted with error: FaultCode: %s; FaultString: %s" code text
    body.Element(XName.Get("listMethodsResponse", XmlNamespace.XRoad)).Elements(XName.Get("service", XmlNamespace.XRoad))
    |> Seq.map (fun service ->
        XRoadServiceIdentifier(
            XRoadMemberIdentifier(
                service.Element(XName.Get("xRoadInstance", XmlNamespace.XRoadIdentifiers)).Value,
                service.Element(XName.Get("memberClass", XmlNamespace.XRoadIdentifiers)).Value,
                service.Element(XName.Get("memberCode", XmlNamespace.XRoadIdentifiers)).Value,
                service.Element(XName.Get("subsystemCode", XmlNamespace.XRoadIdentifiers)) |> Option.ofObj |> Option.map (fun x -> x.Value) |> Option.defaultValue ""
            ),
            service.Element(XName.Get("serviceCode", XmlNamespace.XRoadIdentifiers)).Value,
            service.Element(XName.Get("serviceVersion", XmlNamespace.XRoadIdentifiers)) |> Option.ofObj |> Option.map (fun x -> x.Value) |> Option.defaultValue ""
        )
    )
    |> Seq.toList

let getXDocument (uri: Uri) =
    if uri.Scheme.ToLower() = "https" then
        let request = uri |> createRequest
        use response = request.GetResponse()
        use responseStream = response.GetResponseStream()
        XDocument.Load(responseStream)
    else XDocument.Load(uri.ToString())

/// Check if given uri is valid network location or file path in local file system.
let resolveUri uri =
    match Uri.IsWellFormedUriString(uri, UriKind.Absolute) with
    | true -> Uri(uri, UriKind.Absolute)
    | _ ->
        let fullPath = (FileInfo(uri)).FullName
        match File.Exists(fullPath) with
        | true -> Uri(fullPath)
        | _ -> failwith (sprintf "Cannot resolve url location `%s`" uri)

[<RequireQualifiedAccess>]
module internal MultipartMessage =
    type private ChunkState = Limit | NewLine | EndOfStream

    type private PeekStream(stream: Stream) =
        let mutable borrow = None : int option

        member __.Read() =
            match borrow with
            | Some(x) ->
                borrow <- None
                x
            | None -> stream.ReadByte()

        member __.Peek() =
            match borrow with
            | None ->
                let x = stream.ReadByte()
                borrow <- Some(x)
                x
            | Some(x) -> x

        member __.Flush() =
            stream.Flush()

    let private getBoundaryMarker (response: WebResponse) =
        let parseMultipartContentType (contentType: string) =
            let parts = contentType.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
                        |> List.ofArray
                        |> List.map (fun x -> x.Trim())
            match parts with
            | "multipart/related" :: parts ->
                parts |> List.tryFind (fun x -> x.StartsWith("boundary="))
                      |> Option.map (fun x -> x.Substring(9).Trim('"'))
            | _ -> None
        response
        |> Option.ofObj
        |> Option.map (fun r -> r.ContentType)
        |> Option.bind (parseMultipartContentType)

    let [<Literal>] private CHUNK_SIZE = 4096
    let [<Literal>] private CR = 13
    let [<Literal>] private LF = 10

    let private readChunkOrLine (buffer: byte []) (stream: PeekStream) =
        let rec addByte pos =
            if pos >= CHUNK_SIZE then (Limit, pos)
            else
                match stream.Read() with
                | -1 -> (EndOfStream, pos)
                | byt ->
                    if byt = CR && stream.Peek() = LF then
                        stream.Read() |> ignore
                        (NewLine, pos)
                    else
                        buffer.[pos] <- Convert.ToByte(byt)
                        addByte (pos + 1)
        let result = addByte 0
        stream.Flush()
        result

    let private readLine stream =
        let mutable line: byte[] = [||]
        let buffer = Array.zeroCreate<byte>(CHUNK_SIZE)
        let rec readChunk () =
            let (state, chunkSize) = stream |> readChunkOrLine buffer
            Array.Resize(&line, line.Length + chunkSize)
            Array.Copy(buffer, line, chunkSize)
            match state with
            | Limit -> readChunk()
            | EndOfStream
            | NewLine -> ()
        readChunk()
        line

    let private extractMultipartContentHeaders (stream: PeekStream) =
        let rec getHeaders () = seq {
            match Encoding.ASCII.GetString(stream |> readLine).Trim() with
            | null | "" -> ()
            | line ->
                let (key, value) =
                    match line.Split([| ':' |], 2) with
                    | [| name |] -> (name, "")
                    | [| name; content |] -> (name, content)
                    | _ -> failwith "never"
                yield (key.Trim().ToLower(), value.Trim())
                yield! getHeaders() }
        getHeaders() |> Map.ofSeq

    let private base64Decoder (encoding: Encoding) (encodedBytes: byte []) =
        match encodedBytes with
        | null | [| |] -> [| |]
        | _ ->
            let chars = encoding.GetChars(encodedBytes)
            Convert.FromBase64CharArray(chars, 0, chars.Length)

    let private getDecoder (contentEncoding: string) =
        match contentEncoding.ToLower() with
        | "base64" -> Some(base64Decoder)
        | "quoted-printable" | "7bit" | "8bit" | "binary" -> None
        | _ -> failwithf "No decoder implemented for content transfer encoding `%s`." contentEncoding

    let private startsWith (value: byte []) (buffer: byte []) =
        let rec compare i =
            if value.[i] <> buffer.[i] then false else
            if i = 0 then true else compare (i - 1)
        if buffer |> isNull || value |> isNull || value.Length > buffer.Length then false
        else compare (value.Length - 1)

    let internal read (stream: Stream) (response: WebResponse) : Stream * BinaryContent list =
        match response |> getBoundaryMarker with
        | Some(boundaryMarker) ->
            let stream = PeekStream(stream)
            let contents = ResizeArray<string option * MemoryStream>()
            let isContentMarker = startsWith (Encoding.ASCII.GetBytes (sprintf "--%s" boundaryMarker))
            let isEndMarker = startsWith (Encoding.ASCII.GetBytes (sprintf "--%s--" boundaryMarker))
            let buffer = Array.zeroCreate<byte>(CHUNK_SIZE)
            let rec copyChunk addNewLine encoding (decoder: (Encoding -> byte[] -> byte[]) option) (contentStream: Stream) =
                let (state,size) = stream |> readChunkOrLine buffer
                if buffer |> isEndMarker then false
                elif buffer |> isContentMarker then true
                elif state = EndOfStream then failwith "Unexpected end of multipart stream."
                else
                    if decoder.IsNone && addNewLine then contentStream.Write([| 13uy; 10uy |], 0, 2)
                    let (decodedBuffer,size) = decoder |> Option.fold (fun (buf,_) func -> let buf = buf |> func encoding in (buf,buf.Length)) (buffer,size)
                    contentStream.Write(decodedBuffer, 0, size)
                    match state with EndOfStream -> false | _ -> copyChunk (state = NewLine) encoding decoder contentStream
            let rec parseNextContentPart () =
                let headers = stream |> extractMultipartContentHeaders
                let contentId = headers |> Map.tryFind("content-id") |> Option.map (fun x -> x.Trim().Trim('<', '>'))
                let decoder = headers |> Map.tryFind("content-transfer-encoding") |> Option.bind (getDecoder)
                let contentStream = new MemoryStream()
                contents.Add(contentId, contentStream)
                if copyChunk false Encoding.UTF8 decoder contentStream |> not then ()
                else parseNextContentPart() 
            let rec parseContent () =
                let line = stream |> readLine
                if line |> isEndMarker then ()
                elif line |> isContentMarker then parseNextContentPart()
                else parseContent()
            parseContent()
            match contents |> Seq.toList with
            | (_,content)::attachments ->
                (upcast content, attachments
                                 |> List.map (fun (name,stream) ->
                                    use stream = stream
                                    stream.Position <- 0L
                                    BinaryContent.Create(name.Value, stream.ToArray())))
            | _ -> failwith "empty multipart content"
        | None ->
            let content = new MemoryStream()
            stream.CopyTo(content)
            (upcast content, [])
