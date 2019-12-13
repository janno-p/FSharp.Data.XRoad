namespace FSharp.Data.XRoad

open System
open System.IO

[<RequireQualifiedAccess>]
module internal XmlNamespace =
    let [<Literal>] XRoad = "http://x-road.eu/xsd/xroad.xsd"
    let [<Literal>] XRoadIdentifiers = "http://x-road.eu/xsd/identifiers"

/// Represents identifiers that can be used by the service clients, namely X-Road members and subsystems.
[<AllowNullLiteral>]
type public XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode) =
    new () = XRoadMemberIdentifier("", "", "", "")
    new (xRoadInstance, memberClass, memberCode) = XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, "")

    /// Code identifying the instance of the X-Road system.
    member val XRoadInstance = xRoadInstance with get

    /// Code identifying the member class (e.g., government agency, private enterprise, physical person).
    member val MemberClass = memberClass with get

    /// Member code that uniquely identifies the given X-Road member within its member class.
    member val MemberCode = memberCode with get

    /// Subsystem code is chosen by the X-Road member and it must be unique among the subsystems of this member.
    member val SubsystemCode = subsystemCode with get

    override __.ToString() =
        let owner = sprintf "%s/%s/%s" xRoadInstance memberClass memberCode
        match subsystemCode with null | "" -> sprintf "MEMBER:%s" owner | _ -> sprintf "SUBSYSTEM:%s/%s" owner subsystemCode

    /// Parse XRoadMemberIdentifier from string representation.
    /// Value is expected to be in member (MEMBER:[X-Road instance]/[member class]/[member code]; for example "MEMBER:EE/BUSINESS/123456789")
    /// or subsystem format (SUBSYSTEM:[subsystem owner]/[subsystem code] where subsystem owner is member identifier without prefix; for example "SUBSYSTEM:EE/BUSINESS/123456789/highsecurity").
    static member Parse(value: string) =
        match value.Split([| ':' |], 2) with
        | [| "MEMBER"; value |] ->
            match value.Split('/') with
            | [| xRoadInstance; memberClass; memberCode |] -> XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode)
            | _ -> failwithf "Invalid member identifier: %s" value
        | [| "SUBSYSTEM" ; value |] ->
            match value.Split('/') with
            | [| xRoadInstance; memberClass; memberCode; subsystemCode |] -> XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode)
            | _ -> failwithf "Invalid subsystem identifier: %s" value
        | _ -> failwithf "Invalid owner identifier: %s" value

module internal XRoadHelper =
    let getUUID () = Guid.NewGuid().ToString()

type public ContentEncoding =
    | Binary = 0
    | Base64 = 1

type internal ContentType =
    | FileStorage of FileInfo
    | Data of byte[]

[<AllowNullLiteral>]
type public BinaryContent internal (contentID: string, content: ContentType) =
    member val ContentEncoding = ContentEncoding.Binary with get, set
    member val ContentID = (match contentID with null | "" -> XRoadHelper.getUUID() | _ -> contentID) with get
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

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("FSharp.Data.XRoad.DesignTime.dll")>]
do ()
