namespace FSharp.Data.XRoad

open FSharp.Data.XRoad.Protocol
open FsUnit.Xunit
open FsUnitTyped
open System
open System.IO
open System.Text
open System.Xml.Linq
open Xunit

module private ProtocolTestHelpers =
    let soapEnvNs = XNamespace.Get("http://schemas.xmlsoap.org/soap/envelope/")
    let xroNs = XNamespace.Get("http://x-road.eu/xsd/xroad.xsd")
    let idNs = XNamespace.Get("http://x-road.eu/xsd/identifiers")

    type TestEndpoint(uri) =
        inherit AbstractEndpointDeclaration(uri)

    let makeEndpoint () =
        TestEndpoint(Uri("http://localhost/"))

    let makeTestMethodMap serviceCode serviceVersion =
        { Deserializer = DeserializerDelegate(fun _ _ -> box ())
          Serializer = OperationSerializerDelegate(fun _ _ _ -> ())
          Request = { IsEncoded = false; IsMultipart = false; Accessor = None }
          Response = { IsEncoded = false; IsMultipart = false; Accessor = None }
          ServiceCode = serviceCode
          ServiceVersion = serviceVersion
          Namespaces = []
          RequiredHeaders = dict [(XmlNamespace.XRoad, [|"protocolVersion"; "id"; "client"; "service"|])] }

    let buildEnvelope (endpoint: AbstractEndpointDeclaration) (methodMap: MethodMap) (header: XRoadHeader) =
        use req = new XRoadRequest(endpoint, methodMap, header)
        req.CreateMessage([||])
        use ms = new MemoryStream()
        (req :> IXRoadRequest).Save(ms)
        let bytes = ms.ToArray()
        let str = Encoding.UTF8.GetString(bytes)
        let xmlStart =
            let idx = str.IndexOf("<?xml")
            if idx >= 0 then idx
            else str.IndexOf("<")
        XDocument.Parse(str.Substring(xmlStart))


module SoapEnvelopeTests =
    open ProtocolTestHelpers

    let private makeDefaultHeader () =
        XRoadHeader(
            Client = XRoadMemberIdentifier("EE", "GOV", "70000001", "portal"),
            Producer = XRoadMemberIdentifier("EE", "COM", "90000001", ""),
            ProtocolVersion = "4.0")

    [<Fact>]
    let ``envelope root uses SOAP 1.1 namespace`` () =
        let doc = buildEnvelope (makeEndpoint()) (makeTestMethodMap "svc" None) (makeDefaultHeader())
        doc.Root.Name.NamespaceName |> shouldEqual "http://schemas.xmlsoap.org/soap/envelope/"
        doc.Root.Name.LocalName |> shouldEqual "Envelope"

    [<Fact>]
    let ``envelope has Header and Body child elements`` () =
        let doc = buildEnvelope (makeEndpoint()) (makeTestMethodMap "svc" None) (makeDefaultHeader())
        doc.Root.Element(soapEnvNs + "Header") |> isNull |> shouldEqual false
        doc.Root.Element(soapEnvNs + "Body") |> isNull |> shouldEqual false

    [<Fact>]
    let ``header contains protocolVersion element set to 4.0`` () =
        let doc = buildEnvelope (makeEndpoint()) (makeTestMethodMap "svc" None) (makeDefaultHeader())
        let headerEl = doc.Root.Element(soapEnvNs + "Header")
        let pvEl = headerEl.Element(xroNs + "protocolVersion")
        pvEl |> isNull |> shouldEqual false
        pvEl.Value |> shouldEqual "4.0"

    [<Fact>]
    let ``header id element is a UUID`` () =
        let doc = buildEnvelope (makeEndpoint()) (makeTestMethodMap "svc" None) (makeDefaultHeader())
        let headerEl = doc.Root.Element(soapEnvNs + "Header")
        let idEl = headerEl.Element(xroNs + "id")
        idEl |> isNull |> shouldEqual false
        let mutable guid = Guid.Empty
        Guid.TryParse(idEl.Value, &guid) |> shouldEqual true

    [<Fact>]
    let ``header id is unique per request`` () =
        let mm = makeTestMethodMap "svc" None
        let ep = makeEndpoint()
        let getId (doc: XDocument) = doc.Root.Element(soapEnvNs + "Header").Element(xroNs + "id").Value
        let id1 = getId (buildEnvelope ep mm (makeDefaultHeader()))
        let id2 = getId (buildEnvelope ep mm (makeDefaultHeader()))
        id1 |> should not' (equal id2)

    [<Fact>]
    let ``header contains client element with member identifier`` () =
        let client = XRoadMemberIdentifier("EE", "GOV", "70000001", "portal")
        let header = XRoadHeader(Client = client, Producer = XRoadMemberIdentifier("EE","COM","123",""), ProtocolVersion = "4.0")
        let doc = buildEnvelope (makeEndpoint()) (makeTestMethodMap "svc" None) header
        let clientEl = doc.Root.Element(soapEnvNs + "Header").Element(xroNs + "client")
        clientEl |> isNull |> shouldEqual false
        clientEl.Element(idNs + "xRoadInstance").Value |> shouldEqual "EE"
        clientEl.Element(idNs + "memberClass").Value |> shouldEqual "GOV"
        clientEl.Element(idNs + "memberCode").Value |> shouldEqual "70000001"
        clientEl.Element(idNs + "subsystemCode").Value |> shouldEqual "portal"

    [<Fact>]
    let ``header contains service element with service code and version`` () =
        let mm = makeTestMethodMap "getStatus" (Some "v1")
        let header = XRoadHeader(Client = XRoadMemberIdentifier("EE","GOV","123",""), Producer = XRoadMemberIdentifier("EE","COM","90000001","mySystem"), ProtocolVersion = "4.0")
        let doc = buildEnvelope (makeEndpoint()) mm header
        let serviceEl = doc.Root.Element(soapEnvNs + "Header").Element(xroNs + "service")
        serviceEl |> isNull |> shouldEqual false
        serviceEl.Element(idNs + "serviceCode").Value |> shouldEqual "getStatus"
        serviceEl.Element(idNs + "serviceVersion").Value |> shouldEqual "v1"

    [<Fact>]
    let ``X-Road namespace xro declared on envelope`` () =
        let doc = buildEnvelope (makeEndpoint()) (makeTestMethodMap "svc" None) (makeDefaultHeader())
        let xroAttr = doc.Root.Attribute(XNamespace.Xmlns + "xro")
        xroAttr |> isNull |> shouldEqual false
        xroAttr.Value |> shouldEqual "http://x-road.eu/xsd/xroad.xsd"

    [<Fact>]
    let ``X-Road identifiers namespace declared in header`` () =
        let doc = buildEnvelope (makeEndpoint()) (makeTestMethodMap "svc" None) (makeDefaultHeader())
        let headerEl = doc.Root.Element(soapEnvNs + "Header")
        headerEl.ToString().Contains("http://x-road.eu/xsd/identifiers") |> shouldEqual true

    [<Fact>]
    let ``envelope XML uses UTF-8 encoding declaration`` () =
        let ep = makeEndpoint()
        let mm = makeTestMethodMap "svc" None
        use req = new XRoadRequest(ep, mm, makeDefaultHeader())
        req.CreateMessage([||])
        use ms = new MemoryStream()
        (req :> IXRoadRequest).Save(ms)
        let str = Encoding.UTF8.GetString(ms.ToArray())
        let xmlPart = str.Substring(str.IndexOf("<?xml"))
        xmlPart.ToLower().Contains("utf-8") |> shouldEqual true

    [<Fact>]
    let ``subsystem code omitted when empty`` () =
        let client = XRoadMemberIdentifier("EE", "GOV", "123", "")
        let header = XRoadHeader(Client = client, Producer = XRoadMemberIdentifier("EE","COM","456",""), ProtocolVersion = "4.0")
        let doc = buildEnvelope (makeEndpoint()) (makeTestMethodMap "svc" None) header
        let clientEl = doc.Root.Element(soapEnvNs + "Header").Element(xroNs + "client")
        clientEl.Element(idNs + "subsystemCode") |> isNull |> shouldEqual true

    [<Fact>]
    let ``service version omitted when None`` () =
        let doc = buildEnvelope (makeEndpoint()) (makeTestMethodMap "svc" None) (makeDefaultHeader())
        let serviceEl = doc.Root.Element(soapEnvNs + "Header").Element(xroNs + "service")
        serviceEl.Element(idNs + "serviceVersion") |> isNull |> shouldEqual true


module ClientCertificateTests =
    open ProtocolTestHelpers

    [<Fact>]
    let ``AuthenticationCertificates collection accessible on endpoint`` () =
        let ep = makeEndpoint()
        ep.AuthenticationCertificates |> isNull |> shouldEqual false
        ep.AuthenticationCertificates.Count |> shouldEqual 0

    [<Fact>]
    let ``AcceptedServerCertificate defaults to null`` () =
        let ep = makeEndpoint()
        ep.AcceptedServerCertificate |> isNull |> shouldEqual true

    [<Fact>]
    let ``endpoint with no certificates produces request with empty ClientCertificates`` () =
        let ep = makeEndpoint()
        let mm = makeTestMethodMap "svc" None
        let header = XRoadHeader(Client = XRoadMemberIdentifier("EE","GOV","123",""), Producer = XRoadMemberIdentifier("EE","COM","456",""), ProtocolVersion = "4.0")
        use req = new XRoadRequest(ep, mm, header)
        (req :> IXRoadRequest).HttpWebRequest.ClientCertificates.Count |> shouldEqual 0
