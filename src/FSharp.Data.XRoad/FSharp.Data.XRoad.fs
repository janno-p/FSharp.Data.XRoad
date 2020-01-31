namespace FSharp.Data.XRoad

open NodaTime
open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography.X509Certificates
open System.Xml
open System.Xml.Linq

[<RequireQualifiedAccess>]
module internal XmlNamespace =
    let [<Literal>] Http = "http://schemas.xmlsoap.org/soap/http"
    let [<Literal>] Mime = "http://schemas.xmlsoap.org/wsdl/mime/"
    let [<Literal>] Soap = "http://schemas.xmlsoap.org/wsdl/soap/"
    let [<Literal>] SoapEnc = "http://schemas.xmlsoap.org/soap/encoding/"
    let [<Literal>] SoapEnv = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Wsdl = "http://schemas.xmlsoap.org/wsdl/"
    let [<Literal>] Wsi = "http://ws-i.org/profiles/basic/1.1/xsd"
    let [<Literal>] Xmime = "http://www.w3.org/2005/05/xmlmime"
    let [<Literal>] Xml = "http://www.w3.org/XML/1998/namespace"
    let [<Literal>] Xmlns = "http://www.w3.org/2000/xmlns/";
    let [<Literal>] Xop = "http://www.w3.org/2004/08/xop/include"
    let [<Literal>] XRoad = "http://x-road.eu/xsd/xroad.xsd"
    let [<Literal>] XRoadIdentifiers = "http://x-road.eu/xsd/identifiers"
    let [<Literal>] XRoadRepresentation = "http://x-road.eu/xsd/representation.xsd"
    let [<Literal>] Xsd = "http://www.w3.org/2001/XMLSchema"
    let [<Literal>] Xsi = "http://www.w3.org/2001/XMLSchema-instance"

    /// Defines namespaces which are handled separately (not generated).
    let predefined =
        [ Http; Mime; Soap; SoapEnc; SoapEnv; Wsdl; Wsi; Xmime; Xml; Xmlns; Xop; Xsd; Xsi; XRoad; XRoadIdentifiers; XRoadRepresentation ]

[<AutoOpen>]
module internal Helpers =
    open System.Text.RegularExpressions

    let [<Literal>] CENTRAL_SERVICE_OBJECT_ID = "CENTRALSERVICE"
    let [<Literal>] MEMBER_OBJECT_ID = "MEMBER"
    let [<Literal>] SERVICE_OBJECT_ID = "SERVICE"
    let [<Literal>] SUBSYSTEM_OBJECT_ID = "SUBSYSTEM"

    let (|Regex|_|) pattern input =
        match Regex.Match(input, pattern) with m when m.Success -> Some(input) | _ -> None

    let getUUID () =
        Guid.NewGuid().ToString()

    let getSystemTypeName = function
        | "NodaTime.OffsetDate" -> Some(XmlQualifiedName("date", XmlNamespace.Xsd))
        | "NodaTime.OffsetDateTime" -> Some(XmlQualifiedName("dateTime", XmlNamespace.Xsd))
        | "NodaTime.OffsetTime" -> Some(XmlQualifiedName("time", XmlNamespace.Xsd))
        | "NodaTime.Period" -> Some(XmlQualifiedName("duration", XmlNamespace.Xsd))
        | "System.String" -> Some(XmlQualifiedName("string", XmlNamespace.Xsd))
        | "System.Boolean" -> Some(XmlQualifiedName("boolean", XmlNamespace.Xsd))
        | "System.Decimal" -> Some(XmlQualifiedName("decimal", XmlNamespace.Xsd))
        | "System.Double" -> Some(XmlQualifiedName("double", XmlNamespace.Xsd))
        | "System.Float" -> Some(XmlQualifiedName("float", XmlNamespace.Xsd))
        | "System.Int32" -> Some(XmlQualifiedName("int", XmlNamespace.Xsd))
        | "System.Numerics.BigInteger" -> Some(XmlQualifiedName("integer", XmlNamespace.Xsd))
        | "System.Int64" -> Some(XmlQualifiedName("long", XmlNamespace.Xsd))
        | _ -> None

    let strToOption value =
        match value with null | "" -> None | _ -> Some(value)

/// Represents identifiers of central services.
[<AllowNullLiteral>]
type public XRoadCentralServiceIdentifier(xRoadInstance, serviceCode) =
    /// Code identifying the instance of the X-Road system.
    member val XRoadInstance = xRoadInstance with get

    /// The service code is chosen by the service provider.
    member val ServiceCode = serviceCode with get

    /// X-Road identifier ObjectId for central service identifier
    member val ObjectId = CENTRAL_SERVICE_OBJECT_ID with get

    override __.ToString() =
        sprintf "%s:%s/%s" CENTRAL_SERVICE_OBJECT_ID xRoadInstance serviceCode

    /// Parse XRoadCentralServiceIdentifier from string representation.
    /// Value is expected to be in central service (CENTRALSERVICE:[X-Road instance]/[service code]; for example CENTRALSERVICE:EE/populationRegister_personData) format.
    static member Parse(value: string) =
        match value.Split([| ':' |]) with
        | [| CENTRAL_SERVICE_OBJECT_ID; value |] ->
            match value.Split([| '/' |]) with
            | [| xRoadInstance; serviceCode |] -> XRoadCentralServiceIdentifier(xRoadInstance, serviceCode)
            | _ -> failwithf "Invalid central service identifier: %s" value
        | _ -> failwithf "Invalid central service identifier: %s" value

/// Represents identifiers that can be used by the service clients, namely X-Road members and subsystems.
[<AllowNullLiteral>]
type public XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode) =
    new (xRoadInstance, memberClass, memberCode) = XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, "")

    /// Code identifying the instance of the X-Road system.
    member val XRoadInstance = xRoadInstance with get

    /// Code identifying the member class (e.g., government agency, private enterprise, physical person).
    member val MemberClass = memberClass with get

    /// Member code that uniquely identifies the given X-Road member within its member class.
    member val MemberCode = memberCode with get

    /// Subsystem code is chosen by the X-Road member and it must be unique among the subsystems of this member.
    member val SubsystemCode = subsystemCode with get

    /// X-Road identifier ObjectId for member identifier
    member val ObjectId = (match subsystemCode with null | "" -> MEMBER_OBJECT_ID | _ -> SUBSYSTEM_OBJECT_ID) with get

    override __.ToString() =
        let owner = sprintf "%s/%s/%s" xRoadInstance memberClass memberCode
        match subsystemCode with null | "" -> sprintf "%s:%s" MEMBER_OBJECT_ID owner | _ -> sprintf "%s:%s/%s" SUBSYSTEM_OBJECT_ID owner subsystemCode

    /// Parse XRoadMemberIdentifier from string representation.
    /// Value is expected to be in member (MEMBER:[X-Road instance]/[member class]/[member code]; for example "MEMBER:EE/BUSINESS/123456789")
    /// or subsystem format (SUBSYSTEM:[subsystem owner]/[subsystem code] where subsystem owner is member identifier without prefix; for example "SUBSYSTEM:EE/BUSINESS/123456789/highsecurity").
    static member Parse(value: string) =
        match value.Split([| ':' |], 2) with
        | [| MEMBER_OBJECT_ID; value |] ->
            match value.Split('/') with
            | [| xRoadInstance; memberClass; memberCode |] -> XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode)
            | _ -> failwithf "Invalid member identifier: %s" value
        | [| SUBSYSTEM_OBJECT_ID; value |] ->
            match value.Split('/') with
            | [| xRoadInstance; memberClass; memberCode; subsystemCode |] -> XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode)
            | _ -> failwithf "Invalid subsystem identifier: %s" value
        | _ -> failwithf "Invalid owner identifier: %s" value

/// Represents identifiers of services.
[<AllowNullLiteral>]
type public XRoadServiceIdentifier(owner: XRoadMemberIdentifier, serviceCode, serviceVersion) =
    new (owner: XRoadMemberIdentifier, serviceCode) = XRoadServiceIdentifier(owner, serviceCode, "")

    /// Service owners identifier
    member val Owner = owner with get

    /// The service code is chosen by the service provider.
    member val ServiceCode = serviceCode with get

    /// Version is optional and can be used to distinguish between technically incompatible versions of the same basic service.
    member val ServiceVersion = serviceVersion with get

    /// X-Road identifier ObjectId for service identifier
    member val ObjectId = SERVICE_OBJECT_ID with get

    override __.ToString() =
        let subsystem = match owner.SubsystemCode with null | "" -> "" | subsystemCode -> sprintf "/%s" subsystemCode
        let serviceVersion = match serviceVersion with null | "" -> "" | _ -> sprintf "/%s" serviceVersion
        sprintf "%s:%s/%s/%s%s/%s%s" SERVICE_OBJECT_ID owner.XRoadInstance owner.MemberClass owner.MemberCode subsystem serviceCode serviceVersion

    /// Parse XRoadServiceIdentifier from string representation.
    /// Value is expected to be in member (SERVICE:[service provider]/[service code]/[service version]; for example "SERVICE:EE/BUSINESS/123456789/highsecurity/getSecureData/v1")
    /// where service provider is either member or subsystem identifier without prefix and service version part is optional.
    static member Parse(value: string) =
        match value.Split([| ':' |], 2) with
        | [| SERVICE_OBJECT_ID; value |] ->
            match value.Split('/') with
            | [| xRoadInstance; memberClass; memberCode; serviceCode |] ->
                XRoadServiceIdentifier(XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, ""), serviceCode, "")
            | [| xRoadInstance; memberClass; memberCode; serviceCode; Regex @"^v{\d+}$" serviceVersion |] ->
                XRoadServiceIdentifier(XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, ""), serviceCode, serviceVersion)
            | [| xRoadInstance; memberClass; memberCode; subsystemCode; serviceCode |] ->
                XRoadServiceIdentifier(XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode), serviceCode, "")
            | [| xRoadInstance; memberClass; memberCode; subsystemCode; serviceCode; serviceVersion |] ->
                XRoadServiceIdentifier(XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode), serviceCode, serviceVersion)
            | _ -> failwithf "Invalid member identifier: %s" value
        | _ -> failwithf "Invalid owner identifier: %s" value

/// Combines X-Road SOAP headers for X-Road v6.
[<AllowNullLiteral>]
type public XRoadHeader() =
    // Copy-constructor which initializes new header instance based on given argument.
    new (other: XRoadHeader) as this = XRoadHeader() then
        if not (other |> isNull) then
            this.CentralService <- other.CentralService
            this.Client <- other.Client
            this.Id <- other.Id
            this.Issue <- other.Issue
            this.Producer <- other.Producer
            this.ProtocolVersion <- other.ProtocolVersion
            this.RequestHash <- other.RequestHash
            this.RequestHashAlgorithm <- other.RequestHashAlgorithm
            this.Unresolved <- other.Unresolved
            this.UserId <- other.UserId
    /// Identifies a service client – an entity that initiates the service call.
    member val Client = Unchecked.defaultof<XRoadMemberIdentifier> with get, set
    /// Identifies the service that is invoked by the request.
    member val Producer = Unchecked.defaultof<XRoadMemberIdentifier> with get, set
    /// Identifies the central service that is invoked by the request.
    member val CentralService = Unchecked.defaultof<XRoadCentralServiceIdentifier> with get, set
    /// User whose action initiated the request. The user ID should be prefixed with two-letter ISO country code (e.g., EE12345678901).
    member val UserId = "" with get, set
    /// For responses, this field contains a Base64 encoded hash of the request SOAP message.
    /// This field is automatically filled in by the service provider's security server.
    member val RequestHash = "" with get, set
    /// Identifies the hash algorithm that was used to calculate the value of the requestHash field.
    /// The algorithms are specified as URIs listed in the XML-DSIG specification [DSIG].
    member val RequestHashAlgorithm = "" with get, set
    /// Identifies received application, issue or document that was the cause of the service request.
    /// This field may be used by the client information system to connect service requests (and responses) to working procedures.
    member val Issue = "" with get, set
    /// X-Road message protocol version. The value of this field MUST be 4.0
    member val ProtocolVersion = "" with get, set
    /// Unique identifier for this message. The recommended form of message ID is UUID.
    member val Id = "" with get, set
    /// Unresolved header elements.
    member val Unresolved = ResizeArray<XElement>() with get, set

type public ContentEncoding =
    | Binary = 0
    | Base64 = 1

type internal ContentType =
    | FileStorage of FileInfo
    | Data of byte[]

[<AllowNullLiteral>]
type public BinaryContent internal (contentID: string, content: ContentType) =
    member val ContentEncoding = ContentEncoding.Binary with get, set
    member val ContentID = (match contentID with null | "" -> getUUID() | _ -> contentID) with get
    member __.OpenStream() : Stream =
        match content with
        | FileStorage(file) -> upcast file.OpenRead()
        | Data(data) -> upcast new MemoryStream(data)
    member __.GetBytes() =
        match content with
        | FileStorage(file) -> File.ReadAllBytes(file.FullName)
        | Data(data) -> data
    static member Create(file) = BinaryContent("", FileStorage(file))
    static member Create(contentID, file) = BinaryContent(contentID, FileStorage(file))
    static member Create(data) = BinaryContent("", Data(data))
    static member Create(contentID, data) = BinaryContent(contentID, Data(data))

[<AllowNullLiteral>]
type internal SerializerContext() =
    let attachments = Dictionary<string, BinaryContent>()

    member val DefaultOffset = Offset() with get, set
    member val IsMtomMessage = false with get, set
    member val IsMultipart = false with get, set
    member val Attachments = attachments with get
    member this.AddAttachment(contentID, content, useXop) =
        if useXop then this.IsMtomMessage <- true
        attachments.Add(contentID, content)
    member __.GetAttachment(href: string) =
        if href.StartsWith("cid:") then
            let contentID = href.Substring(4)
            match attachments.TryGetValue(contentID) with
            | true, value -> value
            | _ -> null;
        else failwithf "Invalid multipart content reference: `%s`." href

type internal DeserializerDelegate = delegate of XmlReader * SerializerContext -> obj
type internal SerializerDelegate = delegate of XmlWriter * obj * SerializerContext -> unit
type internal OperationSerializerDelegate = delegate of XmlWriter * obj[] * SerializerContext -> unit

type internal MethodPartMap = {
    IsEncoded: bool
    IsMultipart: bool
    Accessor: XmlQualifiedName option
}

type internal MethodMap = {
    Deserializer: DeserializerDelegate
    Serializer: OperationSerializerDelegate
    Request: MethodPartMap
    Response: MethodPartMap
    ServiceCode: string
    ServiceVersion: string option
    Namespaces: string list
    RequiredHeaders: IDictionary<string, string[]>
}

[<Interface>]
type IXRoadRequest =
    abstract Save: Stream -> unit

[<Interface>]
type IXRoadResponse =
    abstract Save: Stream -> unit

type RequestReadyEventArgs(request: IXRoadRequest, header: XRoadHeader, requestId: string, serviceCode: string, serviceVersion: string) =
    inherit EventArgs()
    member val Request = request with get
    member val RequestId = requestId with get
    member val ServiceCode = serviceCode with get 
    member val ServiceVersion = serviceVersion with get
    member val Header = header with get

type ResponseReadyEventArgs(response: IXRoadResponse, header: XRoadHeader, requestId: string, serviceCode: string, serviceVersion: string) =
    inherit EventArgs()
    member val Response = response with get
    member val RequestId = requestId with get
    member val ServiceCode = serviceCode with get 
    member val ServiceVersion = serviceVersion with get
    member val Header = header with get

type RequestReadyEventHandler = delegate of obj * RequestReadyEventArgs -> unit
type ResponseReadyEventHandler = delegate of obj * ResponseReadyEventArgs -> unit

[<AbstractClass>]
type AbstractEndpointDeclaration (uri: Uri) =
    let requestEvent = Event<RequestReadyEventHandler, RequestReadyEventArgs>()
    let responseEvent = Event<ResponseReadyEventHandler, ResponseReadyEventArgs>()
    let systemTimeZone = DateTimeZoneProviders.Tzdb.GetSystemDefault()

    member val AcceptedServerCertificate = Unchecked.defaultof<X509Certificate> with get, set
    member val AuthenticationCertificates = new ResizeArray<X509Certificate>() with get
    member val DefaultOffset = systemTimeZone.GetUtcOffset(SystemClock.Instance.GetCurrentInstant()) with get, set
    member val Uri = uri with get

    [<CLIEvent>]
    member this.RequestReady = requestEvent.Publish

    [<CLIEvent>]
    member this.ResponseReady = responseEvent.Publish

    member internal this.TriggerRequestReady args = requestEvent.Trigger(this, args)
    member internal this.TriggerResponseReady args = responseEvent.Trigger(this, args)

type MultipartResponse<'TBody> (body: 'TBody, parts: BinaryContent seq) =
    member val Body = body with get
    member val Parts = parts |> Seq.toArray with get

module internal Extensions =
    type XmlReader with
        member this.ReadXsiNullAttribute() =
            let nilValue = (this.GetAttribute("nil", XmlNamespace.Xsi) |> Option.ofObj |> Option.defaultValue "").ToLower()
            nilValue.Equals("1") || nilValue.Equals("true")

        member this.ReadXsiTypeAttribute() =
            match this.GetAttribute("type", XmlNamespace.Xsi) with
            | null -> null
            | typeValue ->
                let qualifiedName = typeValue.Split([| ':' |], 2)
                let nsprefix, nm =
                    match qualifiedName with
                    | [| nm |] -> "", nm
                    | [| nsprefix; nm |] -> nsprefix, nm
                    | _ -> failwith "never"
                let ns = this.LookupNamespace(nsprefix)
                XmlQualifiedName(nm, ns)

        member __.IsQualifiedTypeName(qualifiedName: XmlQualifiedName, nm: string, ns: string, isAnonymous, isDefault) =
            if qualifiedName |> isNull then isAnonymous || isDefault else qualifiedName.Name.Equals(nm) && qualifiedName.Namespace.Equals(ns)

        member this.ReadToNextElement(name, ns, depth, allowContent) =
            while this.Depth > depth do
                if this.NodeType = XmlNodeType.Element && this.Depth = depth + 1 && not allowContent then
                    failwithf "Expected end element of type `%s%s`, but element `%s` was found instead." (match ns with "" -> "" | n -> sprintf "%s:" n) name this.LocalName
                this.Read() |> ignore
            if this.Depth = depth && (this.IsEmptyElement || this.NodeType = XmlNodeType.Element) then
                this.Read() |> ignore

        member this.FindNextStartElement(depth) =
            let rec findNextStartElement () =
                if this.Depth < depth then false
                elif this.Depth = depth && this.NodeType = XmlNodeType.Element then true
                else this.Read() |> ignore; findNextStartElement()
            findNextStartElement()

        member this.IsMatchingElement(name: string, ns: string) =
            name.Equals(this.LocalName) && ns.Equals(this.NamespaceURI)

        member this.MoveToElement(depth, name, ns) =
            while this.Depth < depth do this.Read() |> ignore
            let isElement () = this.Depth = depth && this.NodeType = XmlNodeType.Element && (name |> isNull || (this.LocalName = name && this.NamespaceURI = ns))
            let rec findElement () =
                if isElement() then true
                elif this.Read() then
                    if this.Depth < depth then false
                    else findElement()
                else false
            isElement() || findElement()

module internal MultipartMessage =
    open System.Net
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
            let contents = List<string option * MemoryStream>()
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

module OptionalHelpers =
    let tryGetValue<'t> (expectedId: int) (id: int) (value: obj) =
        if expectedId = id then Optional.Option.Some<'t>(unbox value) else Optional.Option.None<'t>()
