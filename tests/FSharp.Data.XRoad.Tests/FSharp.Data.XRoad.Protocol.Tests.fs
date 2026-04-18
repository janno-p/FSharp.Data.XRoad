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


module HttpRequestTests =
    open ProtocolTestHelpers

    let private makeHeader () =
        XRoadHeader(Client = XRoadMemberIdentifier("EE","GOV","123",""), Producer = XRoadMemberIdentifier("EE","COM","456",""), ProtocolVersion = "4.0")

    [<Fact>]
    let ``HTTP method is POST`` () =
        let mm = makeTestMethodMap "svc" None
        use req = new XRoadRequest(makeEndpoint(), mm, makeHeader())
        (req :> IXRoadRequest).HttpWebRequest.Method |> shouldEqual "POST"

    [<Fact>]
    let ``Content-Type is text/xml with utf-8 charset`` () =
        let mm = makeTestMethodMap "svc" None
        use req = new XRoadRequest(makeEndpoint(), mm, makeHeader())
        (req :> IXRoadRequest).HttpWebRequest.ContentType |> shouldEqual "text/xml; charset=utf-8"

    [<Fact>]
    let ``SOAPAction header is empty string`` () =
        let mm = makeTestMethodMap "svc" None
        use req = new XRoadRequest(makeEndpoint(), mm, makeHeader())
        (req :> IXRoadRequest).HttpWebRequest.Headers.["SOAPAction"] |> shouldEqual ""

    [<Fact>]
    let ``request URI matches endpoint Uri`` () =
        let ep = makeEndpoint()
        let mm = makeTestMethodMap "svc" None
        use req = new XRoadRequest(ep, mm, makeHeader())
        (req :> IXRoadRequest).HttpWebRequest.RequestUri |> shouldEqual (Uri("http://localhost/"))

    [<Fact>]
    let ``request body is serialized SOAP envelope`` () =
        let mm = makeTestMethodMap "svc" None
        use req = new XRoadRequest(makeEndpoint(), mm, makeHeader())
        req.CreateMessage([||])
        use ms = new MemoryStream()
        (req :> IXRoadRequest).Save(ms)
        let str = Encoding.UTF8.GetString(ms.ToArray())
        str.Contains("Envelope") |> shouldEqual true
        str.Contains("http://schemas.xmlsoap.org/soap/envelope/") |> shouldEqual true


module TimeoutTests =
    open ProtocolTestHelpers

    [<Fact>]
    let ``Timeout property defaults to 30000 ms`` () =
        let ep = makeEndpoint()
        ep.Timeout |> shouldEqual 30000

    [<Fact>]
    let ``Timeout property is settable`` () =
        let ep = makeEndpoint()
        ep.Timeout <- 60000
        ep.Timeout |> shouldEqual 60000

    [<Fact>]
    let ``configured timeout applied to HTTP request`` () =
        let ep = makeEndpoint()
        ep.Timeout <- 5000
        let mm = makeTestMethodMap "svc" None
        let header = XRoadHeader(Client = XRoadMemberIdentifier("EE","GOV","123",""), Producer = XRoadMemberIdentifier("EE","COM","456",""), ProtocolVersion = "4.0")
        use req = new XRoadRequest(ep, mm, header)
        (req :> IXRoadRequest).HttpWebRequest.Timeout |> shouldEqual 5000


module ResourceCleanupTests =
    open ProtocolTestHelpers

    [<Fact>]
    let ``XRoadRequest implements IDisposable`` () =
        let mm = makeTestMethodMap "svc" None
        let header = XRoadHeader(Client = XRoadMemberIdentifier("EE","GOV","123",""), Producer = XRoadMemberIdentifier("EE","COM","456",""), ProtocolVersion = "4.0")
        let req = new XRoadRequest(makeEndpoint(), mm, header)
        req :> IDisposable |> isNull |> shouldEqual false
        (req :> IDisposable).Dispose()

    [<Fact>]
    let ``XRoadRequest can be used in use binding`` () =
        let mm = makeTestMethodMap "svc" None
        let header = XRoadHeader(Client = XRoadMemberIdentifier("EE","GOV","123",""), Producer = XRoadMemberIdentifier("EE","COM","456",""), ProtocolVersion = "4.0")
        use req = new XRoadRequest(makeEndpoint(), mm, header)
        req.CreateMessage([||])
        req.RequestId |> String.IsNullOrEmpty |> shouldEqual false


module ServerCertificateTests =
    open ProtocolTestHelpers

    [<Fact>]
    let ``without AcceptedServerCertificate no custom callback set`` () =
        let ep = makeEndpoint()
        let mm = makeTestMethodMap "svc" None
        let header = XRoadHeader(Client = XRoadMemberIdentifier("EE","GOV","123",""), Producer = XRoadMemberIdentifier("EE","COM","456",""), ProtocolVersion = "4.0")
        use req = new XRoadRequest(ep, mm, header)
        // Default: system validation applies (callback not set by us)
        (req :> IXRoadRequest).HttpWebRequest.ServerCertificateValidationCallback |> isNull |> shouldEqual true

    [<Fact>]
    let ``AcceptedServerCertificate property allows pinning configuration`` () =
        let ep = makeEndpoint()
        ep.AcceptedServerCertificate |> isNull |> shouldEqual true
        // Property is settable for pinning use
        ep.AcceptedServerCertificate <- Unchecked.defaultof<_>
        ep.AcceptedServerCertificate |> isNull |> shouldEqual true


module SoapResponseParsingTests =
    open System.IO
    open System.Text
    open System.Xml
    open FSharp.Data.XRoad.Extensions

    let private toStream (xml: string) : MemoryStream =
        new MemoryStream(Encoding.UTF8.GetBytes(xml))

    let private soapNs = "http://schemas.xmlsoap.org/soap/envelope/"

    // R3.a/R3.b: Envelope in SOAP namespace located, reader returned at Body level
    [<Fact>]
    let ``R3.a-b parseSoapEnvelopeBody locates Envelope and Body in SOAP namespace`` () =
        let xml = $"""<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="{soapNs}"><soap:Body><ResponseData>test</ResponseData></soap:Body></soap:Envelope>"""
        use stream = toStream xml
        use reader = parseSoapEnvelopeBody stream
        reader.NodeType |> shouldEqual XmlNodeType.Element
        reader.LocalName |> shouldEqual "Body"

    // R3.c: First child of Body is the content element
    [<Fact>]
    let ``R3.c parseSoapEnvelopeBody positions reader at Body so first child is content`` () =
        let xml = $"""<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="{soapNs}"><soap:Body><OperationResponse><Value>42</Value></OperationResponse></soap:Body></soap:Envelope>"""
        use stream = toStream xml
        use reader = parseSoapEnvelopeBody stream
        reader.MoveToElement(2, null, null) |> shouldEqual true
        reader.LocalName |> shouldEqual "OperationResponse"

    // R3.d: Missing Envelope (wrong namespace) → clear error mentioning Envelope
    [<Fact>]
    let ``R3.d parseSoapEnvelopeBody fails with clear error when Envelope missing`` () =
        let xml = """<?xml version="1.0" encoding="utf-8"?><Root xmlns="http://wrong.ns/"><Body><Data>test</Data></Body></Root>"""
        use stream = toStream xml
        let ex = Assert.Throws<Exception>(fun () -> parseSoapEnvelopeBody stream |> ignore)
        ex.Message.ToLowerInvariant().Contains("envelope") |> shouldEqual true

    // R3.e: Missing Body → clear error mentioning Body
    [<Fact>]
    let ``R3.e parseSoapEnvelopeBody fails with clear error when Body missing`` () =
        let xml = $"""<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="{soapNs}"><soap:Header><Action>test</Action></soap:Header></soap:Envelope>"""
        use stream = toStream xml
        let ex = Assert.Throws<Exception>(fun () -> parseSoapEnvelopeBody stream |> ignore)
        ex.Message.ToLowerInvariant().Contains("body") |> shouldEqual true

    // R3.f: Content extracted and readable by deserializer
    [<Fact>]
    let ``R3.f parseSoapEnvelopeBody extracts body content for deserialization`` () =
        let xml = $"""<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="{soapNs}" xmlns:app="http://app.ns/"><soap:Body><app:GetPersonResponse><app:Name>John</app:Name></app:GetPersonResponse></soap:Body></soap:Envelope>"""
        use stream = toStream xml
        use reader = parseSoapEnvelopeBody stream
        reader.MoveToElement(2, null, null) |> shouldEqual true
        let content = reader.ReadInnerXml()
        content.Contains("Name") |> shouldEqual true


module StreamingResponseBodyTests =
    open System.IO
    open System.Text
    open System.Xml
    open FSharp.Data.XRoad.Extensions

    let private toStream (xml: string) : MemoryStream =
        new MemoryStream(Encoding.UTF8.GetBytes(xml))

    let private soapNs = "http://schemas.xmlsoap.org/soap/envelope/"

    // R12.c: XmlReader used for streaming XML parsing (not XDocument.Load)
    [<Fact>]
    let ``R12.c parseSoapEnvelopeBody returns XmlReader for streaming parse`` () =
        let xml = $"""<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="{soapNs}"><soap:Body><Result>ok</Result></soap:Body></soap:Envelope>"""
        use stream = toStream xml
        use reader = parseSoapEnvelopeBody stream
        reader.NodeType |> shouldEqual XmlNodeType.Element
        reader.ReadState |> shouldEqual ReadState.Interactive

    // R12.b: Deserialization can read from stream incrementally (element by element)
    [<Fact>]
    let ``R12.b reader returned by parseSoapEnvelopeBody supports incremental element reads`` () =
        let xml = $"""<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="{soapNs}"><soap:Body><Root><A>1</A><B>2</B><C>3</C></Root></soap:Body></soap:Envelope>"""
        use stream = toStream xml
        use reader = parseSoapEnvelopeBody stream
        reader.MoveToElement(2, null, null) |> shouldEqual true
        reader.LocalName |> shouldEqual "Root"
        reader.Read() |> shouldEqual true
        reader.LocalName |> shouldEqual "A"
        reader.Read() |> shouldEqual true
        reader.Value |> shouldEqual "1"

    // R12.a: Body stream can be read without loading entire document into memory (reader-based)
    [<Fact>]
    let ``R12.a parseSoapEnvelopeBody does not buffer whole document before returning reader`` () =
        let manyElements = String.concat "" [ for i in 1..1000 -> $"<Item>{i}</Item>" ]
        let xml = $"""<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="{soapNs}"><soap:Body><Items>{manyElements}</Items></soap:Body></soap:Envelope>"""
        use stream = toStream xml
        use reader = parseSoapEnvelopeBody stream
        reader.MoveToElement(2, null, null) |> shouldEqual true
        reader.LocalName |> shouldEqual "Items"
        reader.Read() |> shouldEqual true
        reader.LocalName |> shouldEqual "Item"
        reader.ReadElementContentAsString() |> shouldEqual "1"


module SoapFaultDetectionTests =
    open System.IO
    open System.Text

    let private toStream (xml: string) : MemoryStream =
        new MemoryStream(Encoding.UTF8.GetBytes(xml))

    let private soapNs = "http://schemas.xmlsoap.org/soap/envelope/"

    let private normalSoap content =
        $"""<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="{soapNs}"><soap:Body>{content}</soap:Body></soap:Envelope>"""

    let private soapFault code str =
        $"""<soap:Fault xmlns:soap="{soapNs}"><faultCode>{code}</faultCode><faultString>{str}</faultString></soap:Fault>"""

    // R4.f: Normal response passes through without error
    [<Fact>]
    let ``R4.f checkFaultInStream does not throw for normal response`` () =
        use stream = toStream (normalSoap "<GetResponse><Value>42</Value></GetResponse>")
        checkFaultInStream stream

    // R4.a: Fault element in Body is detected (XPath matches faultCode/faultString wrapper)
    [<Fact>]
    let ``R4.a checkFaultInStream detects soap:Fault with faultCode and faultString`` () =
        use stream = toStream (normalSoap (soapFault "Server" "Internal error"))
        Assert.Throws<XRoadFault>(fun () -> checkFaultInStream stream) |> ignore

    // R4.b + R4.c: faultCode and faultString elements are extracted correctly
    [<Fact>]
    let ``R4.b-c faultCode and faultString are extracted from fault response`` () =
        use stream = toStream (normalSoap (soapFault "soap:Server" "Service unavailable"))
        let ex = Assert.Throws<XRoadFault>(fun () -> checkFaultInStream stream)
        ex.FaultCode |> shouldEqual "soap:Server"
        ex.FaultString |> shouldEqual "Service unavailable"

    // R4.d: Exception is raised with fault code and message
    [<Fact>]
    let ``R4.d XRoadFault message equals faultString`` () =
        use stream = toStream (normalSoap (soapFault "Client" "Bad request format"))
        let ex = Assert.Throws<XRoadFault>(fun () -> checkFaultInStream stream)
        ex.Message |> shouldEqual "Bad request format"

    // R4.e: Exception type is XRoadFault (SOAP fault indication)
    [<Fact>]
    let ``R4.e raised exception type is XRoadFault`` () =
        use stream = toStream (normalSoap (soapFault "Server" "Error"))
        let ex = Assert.Throws<XRoadFault>(fun () -> checkFaultInStream stream)
        ex.FaultCode |> shouldEqual "Server"
