namespace FSharp.Data.XRoad

open System
open System.Security.Cryptography.X509Certificates
open System.IO
open System.Xml.Linq

[<RequireQualifiedAccess>]
module internal XmlNamespace =
    let [<Literal>] SoapEnv = "http://schemas.xmlsoap.org/soap/envelope/"
    let [<Literal>] Xmlns = "http://www.w3.org/2000/xmlns/";
    let [<Literal>] XRoad = "http://x-road.eu/xsd/xroad.xsd"
    let [<Literal>] XRoadIdentifiers = "http://x-road.eu/xsd/identifiers"

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

    member val AcceptedServerCertificate = Unchecked.defaultof<X509Certificate> with get, set
    member val AuthenticationCertificates = new ResizeArray<X509Certificate>() with get
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
