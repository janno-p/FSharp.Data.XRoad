module FSharp.Data.XRoadImplementation

open System
open System.Collections.Concurrent
open System.IO
open System.Net
open System.Reflection
open System.Xml.Linq
open FSharp.Quotations
open FSharp.Core.CompilerServices
open FSharp.Data.XRoad
open FSharp.Data.XRoad.MetaServices
open ProviderImplementation.ProvidedTypes

module internal MultipartMessage =
    open System.Text

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

[<AutoOpen>]
module internal Helpers =
    open System.Collections.Generic
    open System.Text
    open System.Xml

    let [<Literal>] XmlContentType = "text/xml; charset=UTF-8"

    let utf8WithoutBom = UTF8Encoding(false)

    let (|ArrayOf3|) (args: obj array) =
        match args with
        | [| arg1; arg2; arg3 |] -> (unbox<'a> arg1, unbox<'b> arg2, unbox<'c> arg3)
        | _ -> failwith "never"

    let strToOption value =
        match value with null | "" -> None | _ -> Some(value)

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
        request.ContentType <- XmlContentType
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

    let postRequest (stream: Stream) uri =
        let request = uri |> createRequest
        request.Method <- "POST"
        request.ContentType <- XmlContentType
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
                | false, _ -> members.Add(memberClass, new SortedSet<_>([name, memberCode]))
            | "SUBSYSTEM" ->
                let subsystemCode = id.Element(XName.Get("subsystemCode", XmlNamespace.XRoadIdentifiers)).Value
                match subsystems.TryGetValue((memberClass, memberCode)) with
                | true, lst -> lst.Add(subsystemCode) |> ignore
                | false, _ -> subsystems.Add((memberClass, memberCode), new SortedSet<_>([subsystemCode]))
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
            postRequest stream uri
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

[<TypeProvider>]
type XRoadServerProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("FSharp.Data.XRoad.DesignTime", "FSharp.Data.XRoad")], addDefaultProbingLocation=true)

    let ns = "FSharp.Data.XRoad"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<XRoadMemberIdentifier>.Assembly.GetName().Name = asm.GetName().Name)

    let createNoteField message : MemberInfo =
        let field = ProvidedField.Literal("<Note>", typeof<string>, message)
        field.AddXmlDoc(message)
        upcast field

    let createServiceTy securityServerUri (clientId: XRoadMemberIdentifier) (serviceId: XRoadServiceIdentifier) =
        let versionSuffix = serviceId.ServiceVersion |> strToOption |> Option.map (sprintf "/%s") |> Option.defaultValue ""
        let serviceName = sprintf "%s%s" serviceId.ServiceCode versionSuffix
        let serviceTy = ProvidedTypeDefinition(serviceName, Some typeof<obj>, hideObjectMethods=true)
        serviceTy.AddMembersDelayed (fun _ -> [
            let (c1, c2, c3, c4) = (clientId.XRoadInstance, clientId.MemberClass, clientId.MemberCode, clientId.SubsystemCode)
            let (s1, s2, s3, s4, s5, s6) = (serviceId.Owner.XRoadInstance, serviceId.Owner.MemberClass, serviceId.Owner.MemberCode, serviceId.Owner.SubsystemCode, serviceId.ServiceCode, serviceId.ServiceVersion)

            yield ProvidedProperty("Identifier", typeof<XRoadServiceIdentifier>, isStatic=true, getterCode=(fun _ -> <@@ XRoadServiceIdentifier(XRoadMemberIdentifier(s1, s2, s3, s4), s5, s6) @@>)) :> MemberInfo

            yield ProvidedMethod("GetWsdl", [], typeof<string>, isStatic=true, invokeCode=(fun _ -> <@@ downloadWsdl securityServerUri (XRoadMemberIdentifier(c1, c2, c3, c4)) (XRoadServiceIdentifier(XRoadMemberIdentifier(s1, s2, s3, s4), s5, s6)) @@>)) :> MemberInfo

            yield ProvidedField.Literal("IdentifierString", typeof<string>, serviceId.ToString()) :> MemberInfo
            yield ProvidedField.Literal("ServiceCode", typeof<string>, serviceId.ServiceCode) :> MemberInfo

            match serviceId.ServiceVersion |> strToOption with
            | Some(versionValue) ->
                yield ProvidedField.Literal("ServiceVersion", typeof<string>, versionValue) :> MemberInfo
            | None -> ()
        ])
        serviceTy

    let getServicesFromOwner securityServerUri clientId (ownerId: XRoadMemberIdentifier) =
        try
            downloadMethodsList (Uri(securityServerUri)) clientId (XRoadServiceIdentifier(ownerId, "listMethods"))
            |> List.map (fun serviceId -> createServiceTy securityServerUri clientId serviceId :> MemberInfo)
        with e -> [e.ToString() |> createNoteField]

    let createXRoadSubsystemType securityServerUri clientId (subsystemId: XRoadMemberIdentifier) =
        let (xRoadInstance, memberClass, memberCode, subsystemCode) = (subsystemId.XRoadInstance, subsystemId.MemberClass, subsystemId.MemberCode, subsystemId.SubsystemCode)
        let subsystemTy = ProvidedTypeDefinition(subsystemCode, Some typeof<obj>, hideObjectMethods=true)
        subsystemTy.AddXmlDoc (sprintf "Subsystem %s." subsystemCode)
        subsystemTy.AddMembersDelayed(fun _ -> [
            yield ProvidedField.Literal("Name", typeof<string>, subsystemCode) :> MemberInfo
            yield ProvidedProperty("Identifier", typeof<XRoadMemberIdentifier>, isStatic=true, getterCode=(fun _ -> <@@ XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode) @@>)) :> MemberInfo
            yield ProvidedField.Literal("IdentifierString", typeof<string>, subsystemId.ToString()) :> MemberInfo

            match getServicesFromOwner securityServerUri clientId subsystemId with
            | [] -> ()
            | services ->
                let servicesTy = ProvidedTypeDefinition("Services", Some typeof<obj>, hideObjectMethods=true)
                servicesTy.AddXmlDoc(sprintf "Services defined for subsystem %s." subsystemCode)
                servicesTy.AddMembers(services)
                yield servicesTy :> MemberInfo
        ])
        subsystemTy

    let createXRoadMemberType securityServerUri clientId xRoadInstance xRoadMemberClassName (xRoadMember: XRoadMember) =
        let xRoadMemberCode = xRoadMember.Code
        let xRoadMemberId = XRoadMemberIdentifier(xRoadInstance, xRoadMemberClassName, xRoadMember.Code)
        let xRoadMemberTy = ProvidedTypeDefinition(sprintf "%s (%s)" xRoadMember.Name xRoadMember.Code, Some typeof<obj>, hideObjectMethods=true)
        xRoadMemberTy.AddXmlDoc(xRoadMember.Name)
        xRoadMemberTy.AddMembersDelayed(fun _ -> [
            yield ProvidedField.Literal("Name", typeof<string>, xRoadMember.Name) :> MemberInfo
            yield ProvidedField.Literal("Code", typeof<string>, xRoadMember.Code) :> MemberInfo
            yield ProvidedProperty("Identifier", typeof<XRoadMemberIdentifier>, isStatic=true, getterCode=(fun _ -> <@@ XRoadMemberIdentifier(xRoadInstance, xRoadMemberClassName, xRoadMemberCode) @@>)) :> MemberInfo
            yield ProvidedField.Literal("IdentifierString", typeof<string>, XRoadMemberIdentifier(xRoadInstance, xRoadMemberClassName, xRoadMemberCode).ToString()) :> MemberInfo

            match getServicesFromOwner securityServerUri clientId xRoadMemberId with
            | [] -> ()
            | services ->
                let servicesTy = ProvidedTypeDefinition("Services", Some typeof<obj>, hideObjectMethods=true)
                servicesTy.AddXmlDoc(sprintf "Services defined for X-Road member %s (%s)." xRoadMember.Name xRoadMember.Code)
                servicesTy.AddMembers(services)
                yield servicesTy :> MemberInfo

            match xRoadMember.Subsystems with
            | [] -> ()
            | subsystems ->
                let subsystemsTy = ProvidedTypeDefinition("Subsystems", Some typeof<obj>, hideObjectMethods=true)
                subsystemsTy.AddXmlDoc(sprintf "Subsystems defined for X-Road member %s (%s)." xRoadMember.Name xRoadMember.Code)
                subsystemsTy.AddMembersDelayed (fun _ ->
                    subsystems
                    |> List.map (fun subsystem ->
                        let subsystemId = XRoadMemberIdentifier(xRoadMemberId.XRoadInstance, xRoadMemberId.MemberClass, xRoadMemberId.MemberCode, subsystem)
                        createXRoadSubsystemType securityServerUri clientId subsystemId :> MemberInfo
                    )
                )
                yield subsystemsTy :> MemberInfo
        ])
        xRoadMemberTy

    let createXRoadMemberClassType securityServerUri clientId xRoadInstance (xRoadMemberClass: XRoadMemberClass) =
        let xRoadMemberClassTy = ProvidedTypeDefinition(xRoadMemberClass.Name, Some typeof<obj>, hideObjectMethods=true)
        xRoadMemberClassTy.AddXmlDoc(xRoadMemberClass.Name)
        xRoadMemberClassTy.AddMember(ProvidedField.Literal("Name", typeof<string>, xRoadMemberClass.Name))
        xRoadMemberClassTy.AddMembersDelayed (fun _ ->
            xRoadMemberClass.Members |> List.map (fun xRoadMember -> createXRoadMemberType securityServerUri clientId xRoadInstance xRoadMemberClass.Name xRoadMember :> MemberInfo)
        )
        xRoadMemberClassTy

    let createProducersType securityServerUri clientId xRoadInstance forceRefresh =
        let producersTy = ProvidedTypeDefinition("Producers", Some typeof<obj>, hideObjectMethods=true)
        producersTy.AddXmlDoc("All available producers in particular v6 X-Road instance.")
        producersTy.AddMembersDelayed (fun _ ->
            try
                downloadProducerList (Uri(securityServerUri)) xRoadInstance forceRefresh
                |> List.map (fun xRoadMemberClass -> createXRoadMemberClassType securityServerUri clientId xRoadInstance xRoadMemberClass :> MemberInfo)
            with e -> [e.ToString() |> createNoteField]
        )
        producersTy

    let createCentralServicesType securityServerUri xRoadInstance forceRefresh =
        let centralServicesTy = ProvidedTypeDefinition("CentralServices", Some typeof<obj>, hideObjectMethods=true)
        centralServicesTy.AddXmlDoc("All available central services in particular v6 X-Road instance.")
        centralServicesTy.AddMembersDelayed (fun _ ->
            try
                match downloadCentralServiceList securityServerUri xRoadInstance forceRefresh with
                | [] -> [createNoteField "No central services are listed in this X-Road instance."]
                | services ->
                    services |> List.map (fun serviceCode ->
                        let centralServiceType = ProvidedTypeDefinition(serviceCode, Some typeof<obj>, hideObjectMethods=true)
                        centralServiceType.AddMembersDelayed(fun _ -> [
                            yield ProvidedField.Literal("Name", typeof<string>, serviceCode) :> MemberInfo
                            yield ProvidedProperty("Identifier", typeof<XRoadCentralServiceIdentifier>, isStatic=true, getterCode=(fun _ -> <@@ XRoadCentralServiceIdentifier(xRoadInstance, serviceCode) @@>)) :> MemberInfo
                            yield ProvidedField.Literal("IdentifierString", typeof<obj>, XRoadCentralServiceIdentifier(xRoadInstance, serviceCode).ToString()) :> MemberInfo
                        ])
                        upcast centralServiceType
                    )
            with e -> [e.ToString() |> createNoteField]
        )
        centralServicesTy

    let createServerInstanceType typeName (ArrayOf3 (securityServerUriString: string, clientIdentifierString: string, forceRefresh: bool)) =
        let instanceTy = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)

        let clientId = XRoadMemberIdentifier.Parse(clientIdentifierString)
        let xRoadInstance = clientId.XRoadInstance
        let memberClass = clientId.MemberClass
        let memberCode = clientId.MemberCode
        let subsystemCode = clientId.SubsystemCode

        let identifier = ProvidedProperty("Identifier", typeof<XRoadMemberIdentifier>, isStatic=true, getterCode=(fun _ -> <@@ XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode) @@>))
        instanceTy.AddMember(identifier)

        let identifier = ProvidedProperty("Uri", typeof<Uri>, isStatic=true, getterCode=(fun _ -> <@@ Uri(securityServerUriString) @@>))
        instanceTy.AddMember(identifier)

        let identifierString = ProvidedField.Literal("IdentifierString", typeof<string>, clientIdentifierString)
        instanceTy.AddMember(identifierString)

        let uriString = ProvidedField.Literal("UriString", typeof<string>, securityServerUriString)
        instanceTy.AddMember(uriString)

        let instanceField = ProvidedField.Literal("XRoadInstance", typeof<string>, xRoadInstance)
        instanceTy.AddMember(instanceField)

        let memberClassField = ProvidedField.Literal("MemberClass", typeof<string>, memberClass)
        instanceTy.AddMember(memberClassField)

        let memberCodeField = ProvidedField.Literal("MemberCode", typeof<string>, memberCode)
        instanceTy.AddMember(memberCodeField)

        subsystemCode |> strToOption |> Option.iter (fun value ->
            let subsystemCodeField = ProvidedField.Literal("SubsystemCode", typeof<string>, value)
            instanceTy.AddMember(subsystemCodeField)
        )

        let securityServerUri = Uri(securityServerUriString)

        // Type which holds information about producers defined in selected instance.
        instanceTy.AddMember(createProducersType securityServerUriString clientId xRoadInstance forceRefresh)

        // Type which holds information about central services defined in selected instance.
        instanceTy.AddMember(createCentralServicesType securityServerUri xRoadInstance forceRefresh)

        instanceTy

    let createTypes () =
        let serverTy = ProvidedTypeDefinition(asm, ns, "LoadXRoadInstance", Some typeof<obj>)
        serverTy.AddXmlDoc("Type provider which collects data from selected X-Road instance.")

        let staticParameters =
            [ ProvidedStaticParameter("SecurityServerUri", typeof<string>), "X-Road security server uri which is used to connect to that X-Road instance."
              ProvidedStaticParameter("ClientIdentifier", typeof<string>), "Client identifier used to access X-Road infrastructure (MEMBER or SUBSYSTEM)."
              ProvidedStaticParameter("ForceRefresh", typeof<bool>, false), "When `true`, forces type provider to refresh data from security server." ]
            |> List.map (fun (parameter,doc) -> parameter.AddXmlDoc(doc); parameter)

        serverTy.DefineStaticParameters(staticParameters, createServerInstanceType)

        [serverTy]

    do
        this.AddNamespace(ns, createTypes())

[<TypeProvider>]
type BasicErasingProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("FSharp.Data.XRoad.DesignTime", "FSharp.Data.XRoad")], addDefaultProbingLocation=true)

    let ns = "MyNamespace"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<XRoadMemberIdentifier>.Assembly.GetName().Name = asm.GetName().Name)  

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)

        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string>)], invokeCode = fun args -> <@@ (%%(args.[0]):string) :> obj @@>)
        myType.AddMember(ctor2)

        let innerState = ProvidedProperty("InnerState", typeof<string>, getterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
        myType.AddMember(innerState)

        let meth = ProvidedMethod("StaticMethod", [], typeof<XRoadMemberIdentifier>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<XRoadMemberIdentifier>)))
        myType.AddMember(meth)

        let nameOf =
            let param = ProvidedParameter("p", typeof<Expr<int>>)
            param.AddCustomAttribute {
                new CustomAttributeData() with
                    member __.Constructor = typeof<ReflectedDefinitionAttribute>.GetConstructor([||])
                    member __.ConstructorArguments = [||] :> _
                    member __.NamedArguments = [||] :> _
            }
            ProvidedMethod("NameOf", [ param ], typeof<string>, isStatic = true, invokeCode = fun args ->
                <@@
                    match (%%args.[0]) : Expr<int> with
                    | Microsoft.FSharp.Quotations.Patterns.ValueWithName (_, _, n) -> n
                    | e -> failwithf "Invalid quotation argument (expected ValueWithName): %A" e
                @@>)
        myType.AddMember(nameOf)

        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<TypeProvider>]
type BasicGenerativeProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("FSharp.Data.XRoad.DesignTime", "FSharp.Data.XRoad")])

    let ns = "FSharp.Data.XRoad"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<XRoadMemberIdentifier>.Assembly.GetName().Name = asm.GetName().Name)  

    let createType typeName (count:int) =
        let asm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)

        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string>)], invokeCode = fun args -> <@@ (%%(args.[1]):string) :> obj @@>)
        myType.AddMember(ctor2)

        for i in 1 .. count do 
            let prop = ProvidedProperty("Property" + string i, typeof<int>, getterCode = fun args -> <@@ i @@>)
            myType.AddMember(prop)

        let meth = ProvidedMethod("StaticMethod", [], typeof<XRoadMemberIdentifier>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<XRoadMemberIdentifier>)))
        myType.AddMember(meth)
        asm.AddTypes [ myType ]

        myType

    let myParamType = 
        let t = ProvidedTypeDefinition(asm, ns, "GenerativeProvider", Some typeof<obj>, isErased=false)
        t.DefineStaticParameters( [ProvidedStaticParameter("Count", typeof<int>)], fun typeName args -> createType typeName (unbox<int> args.[0]))
        t
    do
        this.AddNamespace(ns, [myParamType])

[<TypeProviderAssembly>]
do ()
