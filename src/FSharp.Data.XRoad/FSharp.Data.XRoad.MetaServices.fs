namespace FSharp.Data.XRoad.MetaServices

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open FSharp.Data.XRoad.Protocol

[<XRoadType(LayoutKind.Sequence, IsAnonymous = true)>]
type GetWsdl () =
    [<XRoadElement("serviceCode", Namespace = XmlNamespace.XRoad)>] member val ServiceCode = Unchecked.defaultof<string> with get, set
    [<XRoadElement("serviceVersion", Namespace = XmlNamespace.XRoad)>] member val ServiceVersion = Optional.Option.None<string>() with get, set

[<XRoadType(LayoutKind.Sequence, IsAnonymous = true)>]
type GetWsdlResponse () =
    [<XRoadElement("serviceCode", Namespace = XmlNamespace.XRoad)>] member val ServiceCode = Unchecked.defaultof<string> with get, set
    [<XRoadElement("serviceVersion", Namespace = XmlNamespace.XRoad)>] member val ServiceVersion = Optional.Option.None<string>() with get, set

type internal MetaServicesEndpoint (uri) =
    inherit AbstractEndpointDeclaration (uri)

    [<XRoadOperation("getWsdl", null, ProtocolVersion = "4.0")>]
    [<XRoadRequiredHeaders(XmlNamespace.XRoad, "client", "service", "userId", "id", "protocolVersion")>]
    [<XRoadRequest("getWsdl", XmlNamespace.XRoad)>]
    [<XRoadResponse("getWsdlResponse", XmlNamespace.XRoad, ReturnType = typeof<GetWsdlResponse>)>]
    member this.GetWsdl(header: XRoadHeader, [<XRoadElementAttribute(MergeContent = true)>] request: GetWsdl) : MultipartResponse<GetWsdlResponse> =
        XRoadUtil.MakeServiceCall(this, "GetWsdl", header, [| request |]) |> unbox

[<AutoOpen>]
module Runtime =
    open System
    open System.IO

    let internal openWsdlStream uri (clientId: XRoadMemberIdentifier) (serviceId: XRoadServiceIdentifier) : Stream =
        let serviceVersion = match serviceId.ServiceVersion with "" -> Optional.Option.None<_>() | value -> Optional.Option.Some<_>(value)
        let header = XRoadHeader(Client = clientId, Producer = serviceId.Owner, ProtocolVersion = "4.0", UserId = "")
        let request = GetWsdl(ServiceCode = serviceId.ServiceCode, ServiceVersion = serviceVersion)
        let endpoint = MetaServicesEndpoint(Uri(uri))
        let response = endpoint.GetWsdl(header, request)
        response.Parts.[0].OpenStream()

    let downloadWsdl uri (client: XRoadMemberIdentifier) (service: XRoadServiceIdentifier) =
        use stream = openWsdlStream uri client service
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

type X () =
    static let someMethod = typeof<Optional.Option>.GetMethods() |> Seq.filter (fun m -> m.Name = "Some" && m.GetGenericArguments().Length = 1) |> Seq.exactlyOne

    static member MakeOptionalSome (value: obj, t: System.Type) =
        let someMethod = someMethod.MakeGenericMethod(t);
        someMethod.Invoke(null, [| value |])