namespace FSharp.Data.XRoad.MetaServices

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open FSharp.Data.XRoad.Protocol
open System.Net.Http


[<XRoadType(LayoutKind.Sequence, IsAnonymous = true)>]
type GetWsdl () =
    [<XRoadElement("serviceCode", Namespace = XmlNamespace.XRoad)>]
    member val ServiceCode = Unchecked.defaultof<string> with get, set

    [<XRoadElement("serviceVersion", Namespace = XmlNamespace.XRoad)>]
    member val ServiceVersion = Optional.Option.None<string>() with get, set


[<XRoadType(LayoutKind.Sequence, IsAnonymous = true)>]
type GetWsdlResponse () =
    [<XRoadElement("serviceCode", Namespace = XmlNamespace.XRoad)>]
    member val ServiceCode = Unchecked.defaultof<string> with get, set

    [<XRoadElement("serviceVersion", Namespace = XmlNamespace.XRoad)>]
    member val ServiceVersion = Optional.Option.None<string>() with get, set


type internal MetaServicesEndpoint (httpClient: HttpClient) =
    inherit AbstractEndpointDeclaration (httpClient)

    [<XRoadOperation("getWsdl", null, ProtocolVersion = "4.0")>]
    [<XRoadRequiredHeaders(XmlNamespace.XRoad, "client", "service", "userId", "id", "protocolVersion")>]
    [<XRoadRequest("getWsdl", XmlNamespace.XRoad)>]
    [<XRoadResponse("getWsdlResponse", XmlNamespace.XRoad, ReturnType = typeof<GetWsdlResponse>)>]
    member this.GetWsdl(header: XRoadHeader, [<XRoadElement(MergeContent = true)>] request: GetWsdl) : MultipartResponse<GetWsdlResponse> =
        XRoadUtil.MakeServiceCall(this, "GetWsdl", header, [| request |]) |> unbox


[<AutoOpen>]
module Runtime =
    open System.IO

    let internal openWsdlStream (clientId: XRoadMemberIdentifier) (serviceId: XRoadServiceIdentifier) httpClient : Stream =
        let serviceVersion = match serviceId.ServiceVersion with "" -> Optional.Option.None<_>() | value -> Optional.Option.Some<_>(value)
        let header = XRoadHeader(Client = clientId, Producer = serviceId.Owner, ProtocolVersion = "4.0", UserId = "")
        let request = GetWsdl(ServiceCode = serviceId.ServiceCode, ServiceVersion = serviceVersion)
        let endpoint = MetaServicesEndpoint(httpClient)
        let response = endpoint.GetWsdl(header, request)
        response.Parts[0].OpenStream()

    let downloadWsdl (client: XRoadMemberIdentifier) (service: XRoadServiceIdentifier) httpClient =
        use stream = httpClient |> openWsdlStream client service
        use reader = new StreamReader(stream)
        reader.ReadToEnd()
