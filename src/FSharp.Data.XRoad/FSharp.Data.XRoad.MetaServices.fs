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
    member this.GetWsdlAsync(header: XRoadHeader, [<XRoadElement(MergeContent = true)>] request: GetWsdl, cancellationToken) =
        XRoadUtil.MakeServiceCall(
            this,
            "GetWsdl",
            header,
            [| request |],
            id,
            (fun (x, att) -> MultipartResponse<GetWsdlResponse>(unbox x, att)),
            cancellationToken
        )

[<AutoOpen>]
module Runtime =
    open System.Net.Http
    open System.IO

    let internal openWsdlStream (httpClient: HttpClient, clientId: XRoadMemberIdentifier, serviceId: XRoadServiceIdentifier, cancellationToken) =
        task {
            let serviceVersion = match serviceId.ServiceVersion with "" -> Optional.Option.None<_>() | value -> Optional.Option.Some<_>(value)
            let header = XRoadHeader(Client = clientId, Producer = serviceId.Owner, ProtocolVersion = "4.0", UserId = "")
            let request = GetWsdl(ServiceCode = serviceId.ServiceCode, ServiceVersion = serviceVersion)
            let endpoint = MetaServicesEndpoint(httpClient)
            let! response = endpoint.GetWsdlAsync(header, request, cancellationToken)
            return response.Parts[0].OpenStream()
        }

    let downloadWsdl (httpClient: HttpClient, client: XRoadMemberIdentifier, service: XRoadServiceIdentifier, cancellationToken) =
        task {
            use! stream = openWsdlStream (httpClient, client, service, cancellationToken)
            use reader = new StreamReader(stream)
            return! reader.ReadToEndAsync()
        }
