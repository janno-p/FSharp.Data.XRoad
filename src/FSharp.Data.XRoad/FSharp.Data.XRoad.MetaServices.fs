namespace FSharp.Data.XRoad.MetaServices

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes

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

    [<XRoadOperation("getWsdl", null, XRoadProtocol.Version40, ProtocolVersion = "4.0")>]
    [<XRoadRequiredHeaders(XmlNamespace.XRoad, "client", "service", "userId", "id", "protocolVersion")>]
    [<XRoadRequest("getWsdl", XmlNamespace.XRoad)>]
    [<XRoadResponse("getWsdlResponse", XmlNamespace.XRoad, ReturnType = typeof<GetWsdlResponse>)>]
    member this.GetWsdl(header: XRoadHeader, [<XRoadElementAttribute(MergeContent = true)>] request: GetWsdl) : MultipartResponse<GetWsdlResponse> =
        //XRoadUtil.MakeServiceCall(this, "GetWsdl", header, [| request |]) |> unbox
        MultipartResponse<GetWsdlResponse>(GetWsdlResponse(), [])
