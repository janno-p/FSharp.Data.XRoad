namespace FSharp.Data.XRoad

open FsUnit.Xunit
open FsUnitTyped
open System
open System.IO
open Xunit

module XRoadMemberIdentifierTests =
    [<Fact>]
    let ``has four properties`` () =
        let id = XRoadMemberIdentifier("EE", "GOV", "70000001", "subsys")
        id.XRoadInstance |> shouldEqual "EE"
        id.MemberClass |> shouldEqual "GOV"
        id.MemberCode |> shouldEqual "70000001"
        id.SubsystemCode |> shouldEqual "subsys"

    [<Fact>]
    let ``subsystem empty means member level`` () =
        let id = XRoadMemberIdentifier("EE", "GOV", "70000001", "")
        id.ObjectId |> shouldEqual "MEMBER"

    [<Fact>]
    let ``subsystem present means subsystem level`` () =
        let id = XRoadMemberIdentifier("EE", "GOV", "70000001", "subsys")
        id.ObjectId |> shouldEqual "SUBSYSTEM"

    [<Fact>]
    let ``ToString member format`` () =
        let id = XRoadMemberIdentifier("EE", "GOV", "70000001", "")
        id.ToString() |> shouldEqual "MEMBER:EE/GOV/70000001"

    [<Fact>]
    let ``ToString subsystem format`` () =
        let id = XRoadMemberIdentifier("EE", "GOV", "70000001", "subsys")
        id.ToString() |> shouldEqual "SUBSYSTEM:EE/GOV/70000001/subsys"

    [<Fact>]
    let ``Parse member round-trip`` () =
        let id = XRoadMemberIdentifier.Parse("MEMBER:EE/GOV/70000001")
        id.XRoadInstance |> shouldEqual "EE"
        id.MemberClass |> shouldEqual "GOV"
        id.MemberCode |> shouldEqual "70000001"
        id.SubsystemCode |> shouldEqual ""

    [<Fact>]
    let ``Parse subsystem round-trip`` () =
        let id = XRoadMemberIdentifier.Parse("SUBSYSTEM:EE/GOV/70000001/subsys")
        id.SubsystemCode |> shouldEqual "subsys"

    [<Fact>]
    let ``Parse invalid format throws`` () =
        (fun () -> XRoadMemberIdentifier.Parse("INVALID:EE") |> ignore)
        |> should throw typeof<Exception>

    [<Fact>]
    let ``equality same values`` () =
        let a = XRoadMemberIdentifier("EE", "GOV", "123", "sub")
        let b = XRoadMemberIdentifier("EE", "GOV", "123", "sub")
        a.Equals(b) |> shouldEqual true

    [<Fact>]
    let ``equality different values`` () =
        let a = XRoadMemberIdentifier("EE", "GOV", "123", "sub")
        let b = XRoadMemberIdentifier("EE", "GOV", "999", "sub")
        a.Equals(b) |> shouldEqual false

    [<Fact>]
    let ``op_Equality same values`` () =
        let a = XRoadMemberIdentifier("EE", "GOV", "123", "")
        let b = XRoadMemberIdentifier("EE", "GOV", "123", "")
        XRoadMemberIdentifier.op_Equality(a, b) |> shouldEqual true

    [<Fact>]
    let ``op_Equality both null`` () =
        XRoadMemberIdentifier.op_Equality(null, null) |> shouldEqual true

    [<Fact>]
    let ``op_Equality one null`` () =
        let a = XRoadMemberIdentifier("EE", "GOV", "123", "")
        XRoadMemberIdentifier.op_Equality(a, null) |> shouldEqual false

    [<Fact>]
    let ``GetHashCode equal for same values`` () =
        let a = XRoadMemberIdentifier("EE", "GOV", "123", "sub")
        let b = XRoadMemberIdentifier("EE", "GOV", "123", "sub")
        a.GetHashCode() |> shouldEqual (b.GetHashCode())

module XRoadCentralServiceIdentifierTests =
    [<Fact>]
    let ``has XRoadInstance and ServiceCode`` () =
        let id = XRoadCentralServiceIdentifier("EE", "populationRegister")
        id.XRoadInstance |> shouldEqual "EE"
        id.ServiceCode |> shouldEqual "populationRegister"

    [<Fact>]
    let ``ObjectId returns CENTRALSERVICE`` () =
        XRoadCentralServiceIdentifier("EE", "svc").ObjectId |> shouldEqual "CENTRALSERVICE"

    [<Fact>]
    let ``ToString canonical format`` () =
        XRoadCentralServiceIdentifier("EE", "svc").ToString()
        |> shouldEqual "CENTRALSERVICE:EE/svc"

    [<Fact>]
    let ``Parse round-trip`` () =
        let id = XRoadCentralServiceIdentifier.Parse("CENTRALSERVICE:EE/svc")
        id.XRoadInstance |> shouldEqual "EE"
        id.ServiceCode |> shouldEqual "svc"

    [<Fact>]
    let ``Parse invalid throws`` () =
        (fun () -> XRoadCentralServiceIdentifier.Parse("INVALID:x") |> ignore)
        |> should throw typeof<Exception>

    [<Fact>]
    let ``equality same values`` () =
        let a = XRoadCentralServiceIdentifier("EE", "svc")
        let b = XRoadCentralServiceIdentifier("EE", "svc")
        a.Equals(b) |> shouldEqual true

    [<Fact>]
    let ``equality different values`` () =
        let a = XRoadCentralServiceIdentifier("EE", "svc1")
        let b = XRoadCentralServiceIdentifier("EE", "svc2")
        a.Equals(b) |> shouldEqual false

    [<Fact>]
    let ``op_Equality both null`` () =
        XRoadCentralServiceIdentifier.op_Equality(null, null) |> shouldEqual true

    [<Fact>]
    let ``GetHashCode equal for same values`` () =
        let a = XRoadCentralServiceIdentifier("EE", "svc")
        let b = XRoadCentralServiceIdentifier("EE", "svc")
        a.GetHashCode() |> shouldEqual (b.GetHashCode())

module BinaryContentTests =
    [<Fact>]
    let ``Create from byte array`` () =
        let data = [| 1uy; 2uy; 3uy |]
        let bc = BinaryContent.Create(data)
        bc.GetBytes() |> shouldEqual data

    [<Fact>]
    let ``Create from byte array with ContentID`` () =
        let bc = BinaryContent.Create("myid", [| 1uy |])
        bc.ContentID |> shouldEqual "myid"

    [<Fact>]
    let ``ContentID defaults to UUID when empty`` () =
        let bc = BinaryContent.Create([| 1uy |])
        bc.ContentID |> should not' (be EmptyString)
        Guid.TryParse(bc.ContentID) |> fst |> shouldEqual true

    [<Fact>]
    let ``Create from stream reads data`` () =
        let data = [| 10uy; 20uy; 30uy |]
        use ms = new MemoryStream(data)
        let bc = BinaryContent.Create(ms :> Stream)
        bc.GetBytes() |> shouldEqual data

    [<Fact>]
    let ``Create from stream with ContentID`` () =
        let data = [| 7uy; 8uy |]
        use ms = new MemoryStream(data)
        let bc = BinaryContent.Create("cid", ms :> Stream)
        bc.ContentID |> shouldEqual "cid"
        bc.GetBytes() |> shouldEqual data

    [<Fact>]
    let ``OpenStream returns readable stream`` () =
        let data = [| 5uy; 6uy; 7uy |]
        let bc = BinaryContent.Create(data)
        use s = bc.OpenStream()
        let buf = Array.zeroCreate data.Length
        s.Read(buf, 0, buf.Length) |> ignore
        buf |> shouldEqual data

module OptionalHelpersTests =
    [<Fact>]
    let ``tryGetValue matching id returns Some`` () =
        OptionalHelpers.tryGetValue 1 1 "hello"
        |> shouldEqual (Optional.Option.Some<string>("hello"))

    [<Fact>]
    let ``tryGetValue non-matching id returns None`` () =
        OptionalHelpers.tryGetValue 2 1 "hello"
        |> shouldEqual (Optional.Option.None<string>())

module XRoadServiceIdentifierTests =
    [<Fact>]
    let ``has Owner ServiceCode ServiceVersion`` () =
        let owner = XRoadMemberIdentifier("EE", "GOV", "123", "sub")
        let svc = XRoadServiceIdentifier(owner, "getSomething", "v1")
        svc.Owner |> shouldEqual owner
        svc.ServiceCode |> shouldEqual "getSomething"
        svc.ServiceVersion |> shouldEqual "v1"

    [<Fact>]
    let ``ServiceVersion optional empty string`` () =
        let owner = XRoadMemberIdentifier("EE", "GOV", "123", "")
        let svc = XRoadServiceIdentifier(owner, "getSomething")
        svc.ServiceVersion |> shouldEqual ""

    [<Fact>]
    let ``ObjectId returns SERVICE`` () =
        let owner = XRoadMemberIdentifier("EE", "GOV", "123", "")
        XRoadServiceIdentifier(owner, "svc").ObjectId |> shouldEqual "SERVICE"

    [<Fact>]
    let ``equality same values`` () =
        let owner = XRoadMemberIdentifier("EE", "GOV", "123", "sub")
        let a = XRoadServiceIdentifier(owner, "svc", "v1")
        let b = XRoadServiceIdentifier(XRoadMemberIdentifier("EE", "GOV", "123", "sub"), "svc", "v1")
        a.Equals(b) |> shouldEqual true

    [<Fact>]
    let ``equality different service code`` () =
        let owner = XRoadMemberIdentifier("EE", "GOV", "123", "")
        let a = XRoadServiceIdentifier(owner, "svc1", "")
        let b = XRoadServiceIdentifier(owner, "svc2", "")
        a.Equals(b) |> shouldEqual false

    [<Fact>]
    let ``op_Equality both null`` () =
        XRoadServiceIdentifier.op_Equality(null, null) |> shouldEqual true

module XRoadHeaderTests =
    [<Fact>]
    let ``Id defaults to non-empty UUID`` () =
        let h = XRoadHeader()
        h.Id |> should not' (be EmptyString)
        Guid.TryParse(h.Id) |> fst |> shouldEqual true

    [<Fact>]
    let ``two instances get different default IDs`` () =
        let a = XRoadHeader()
        let b = XRoadHeader()
        a.Id |> should not' (equal b.Id)

    [<Fact>]
    let ``properties readable and writable`` () =
        let h = XRoadHeader()
        h.ProtocolVersion <- "4.0"
        h.ProtocolVersion |> shouldEqual "4.0"

    [<Fact>]
    let ``Client property writable`` () =
        let h = XRoadHeader()
        let client = XRoadMemberIdentifier("EE", "GOV", "123", "sub")
        h.Client <- client
        h.Client |> shouldEqual client

    [<Fact>]
    let ``Producer property writable`` () =
        let h = XRoadHeader()
        let producer = XRoadMemberIdentifier("EE", "GOV", "123", "sub")
        h.Producer <- producer
        h.Producer |> shouldEqual producer

    [<Fact>]
    let ``ToString includes Id Client Producer and ProtocolVersion`` () =
        let h = XRoadHeader()
        h.ProtocolVersion <- "4.0"
        let s = h.ToString()
        s.Contains("Id=") |> shouldEqual true
        s.Contains("Client=") |> shouldEqual true
        s.Contains("Producer=") |> shouldEqual true
        s.Contains("ProtocolVersion=4.0") |> shouldEqual true

module IdentifierParsingRobustnessTests =
    [<Fact>]
    let ``TryParse member returns Ok for valid input`` () =
        match XRoadMemberIdentifier.TryParse("MEMBER:EE/GOV/70000001") with
        | Ok id -> id.MemberCode |> shouldEqual "70000001"
        | Error msg -> failwith msg

    [<Fact>]
    let ``TryParse member returns Error for invalid input`` () =
        match XRoadMemberIdentifier.TryParse("INVALID:EE") with
        | Ok _ -> failwith "Expected Error"
        | Error msg -> msg |> should not' (be EmptyString)

    [<Fact>]
    let ``TryParse member error message includes format example`` () =
        match XRoadMemberIdentifier.TryParse("WRONG") with
        | Ok _ -> failwith "Expected Error"
        | Error msg -> msg.Contains("MEMBER:") |> shouldEqual true

    [<Fact>]
    let ``TryParse member trims whitespace`` () =
        match XRoadMemberIdentifier.TryParse("  MEMBER:EE/GOV/123  ") with
        | Ok id -> id.MemberCode |> shouldEqual "123"
        | Error msg -> failwith msg

    [<Fact>]
    let ``TryParse subsystem returns Ok`` () =
        match XRoadMemberIdentifier.TryParse("SUBSYSTEM:EE/GOV/123/sub") with
        | Ok id -> id.SubsystemCode |> shouldEqual "sub"
        | Error msg -> failwith msg

    [<Fact>]
    let ``TryParse subsystem returns Error for partial prefix`` () =
        match XRoadMemberIdentifier.TryParse("SUBSYSTEM:EE/GOV") with
        | Ok _ -> failwith "Expected Error"
        | Error msg -> msg.Contains("SUBSYSTEM:") |> shouldEqual true

    [<Fact>]
    let ``TryParse central service returns Ok`` () =
        match XRoadCentralServiceIdentifier.TryParse("CENTRALSERVICE:EE/svc") with
        | Ok id -> id.ServiceCode |> shouldEqual "svc"
        | Error msg -> failwith msg

    [<Fact>]
    let ``TryParse central service returns Error with example`` () =
        match XRoadCentralServiceIdentifier.TryParse("INVALID") with
        | Ok _ -> failwith "Expected Error"
        | Error msg -> msg.Contains("CENTRALSERVICE:") |> shouldEqual true

    [<Fact>]
    let ``TryParse central service trims whitespace`` () =
        match XRoadCentralServiceIdentifier.TryParse("  CENTRALSERVICE:EE/svc  ") with
        | Ok id -> id.XRoadInstance |> shouldEqual "EE"
        | Error msg -> failwith msg

    [<Fact>]
    let ``Parse still throws on invalid input`` () =
        (fun () -> XRoadMemberIdentifier.Parse("INVALID:X") |> ignore)
        |> should throw typeof<Exception>

    [<Fact>]
    let ``TryParse service member-level returns Ok`` () =
        match XRoadServiceIdentifier.TryParse("SERVICE:EE/GOV/123/getSomething") with
        | Ok id -> id.ServiceCode |> shouldEqual "getSomething"
        | Error msg -> failwith msg

    [<Fact>]
    let ``TryParse service subsystem-level returns Ok`` () =
        match XRoadServiceIdentifier.TryParse("SERVICE:EE/GOV/123/sub/getSomething/v1") with
        | Ok id ->
            id.ServiceCode |> shouldEqual "getSomething"
            id.ServiceVersion |> shouldEqual "v1"
        | Error msg -> failwith msg

    [<Fact>]
    let ``TryParse service invalid prefix returns Error with example`` () =
        match XRoadServiceIdentifier.TryParse("BAD:EE/GOV/123/svc") with
        | Ok _ -> failwith "Expected Error"
        | Error msg -> msg.Contains("SERVICE:") |> shouldEqual true

    [<Fact>]
    let ``Case sensitivity preserved in parsed values`` () =
        match XRoadMemberIdentifier.TryParse("MEMBER:EE/GOV/MixedCode") with
        | Ok id -> id.MemberCode |> shouldEqual "MixedCode"
        | Error msg -> failwith msg

    [<Fact>]
    let ``TryParse member empty XRoadInstance returns Error`` () =
        match XRoadMemberIdentifier.TryParse("MEMBER:/GOV/123") with
        | Ok _ -> failwith "Expected Error for empty XRoadInstance"
        | Error _ -> ()

    [<Fact>]
    let ``TryParse member empty MemberClass returns Error`` () =
        match XRoadMemberIdentifier.TryParse("MEMBER:EE//123") with
        | Ok _ -> failwith "Expected Error for empty MemberClass"
        | Error _ -> ()

    [<Fact>]
    let ``TryParse member empty MemberCode returns Error`` () =
        match XRoadMemberIdentifier.TryParse("MEMBER:EE/GOV/") with
        | Ok _ -> failwith "Expected Error for empty MemberCode"
        | Error _ -> ()

    [<Fact>]
    let ``TryParse service empty memberCode returns Error`` () =
        match XRoadServiceIdentifier.TryParse("SERVICE:EE/GOV//getSomething") with
        | Ok _ -> failwith "Expected Error for empty memberCode"
        | Error _ -> ()

    [<Fact>]
    let ``TryParse service member-level with version returns correct version`` () =
        match XRoadServiceIdentifier.TryParse("SERVICE:EE/GOV/70000001/getService/v1") with
        | Ok id ->
            id.ServiceVersion |> shouldEqual "v1"
            id.Owner.SubsystemCode |> shouldEqual ""
            id.ServiceCode |> shouldEqual "getService"
        | Error msg -> failwith msg

    [<Fact>]
    let ``TryParse service member-level with v10 version returns correct version`` () =
        match XRoadServiceIdentifier.TryParse("SERVICE:EE/GOV/70000001/getService/v10") with
        | Ok id -> id.ServiceVersion |> shouldEqual "v10"
        | Error msg -> failwith msg

module AbstractEndpointDeclarationTests =
    open System
    open System.Net.Http

    type TestEndpoint(uri) =
        inherit AbstractEndpointDeclaration(uri)

    [<Fact>]
    let ``has Uri property`` () =
        let ep = TestEndpoint(Uri("http://example.com/"))
        ep.Uri |> shouldEqual (Uri("http://example.com/"))

    [<Fact>]
    let ``has AuthenticationCertificates list`` () =
        let ep = TestEndpoint(Uri("http://example.com/"))
        ep.AuthenticationCertificates |> should not' (be Null)

    [<Fact>]
    let ``HttpClientFactory defaults to null`` () =
        let ep = TestEndpoint(Uri("http://example.com/"))
        ep.HttpClientFactory |> should be Null

    [<Fact>]
    let ``HttpClientFactory setter accepts implementation`` () =
        let ep = TestEndpoint(Uri("http://example.com/"))
        let factory = { new IXRoadHttpClientFactory with
                            member _.CreateHttpClient(name) = new HttpClient() }
        ep.HttpClientFactory <- factory
        ep.HttpClientFactory |> should not' (be Null)

    [<Fact>]
    let ``IXRoadHttpClientFactory interface has CreateHttpClient method`` () =
        let methods = typeof<IXRoadHttpClientFactory>.GetMethods()
        methods |> Array.exists (fun m -> m.Name = "CreateHttpClient") |> shouldEqual true

module ChoiceTypeTests =
    open FSharp.Data.XRoad.Choices

    [<Fact>]
    let ``IChoiceOf1 through IChoiceOf8 exist`` () =
        typeof<IChoiceOf1<string>> |> should not' (be Null)
        typeof<IChoiceOf2<string, int>> |> should not' (be Null)
        typeof<IChoiceOf3<string, int, bool>> |> should not' (be Null)
        typeof<IChoiceOf8<string, int, bool, float, string, int, bool, float>> |> should not' (be Null)

    [<Fact>]
    let ``IChoiceOf1 has TryGetOption1 method`` () =
        let methods = typeof<IChoiceOf1<string>>.GetMethods()
        methods |> Array.exists (fun m -> m.Name = "TryGetOption1") |> shouldEqual true

    [<Fact>]
    let ``IChoiceOf2 has TryGetOption1 and TryGetOption2`` () =
        let methods = typeof<IChoiceOf2<string,int>>.GetMethods()
        methods |> Array.exists (fun m -> m.Name = "TryGetOption1") |> shouldEqual true
        methods |> Array.exists (fun m -> m.Name = "TryGetOption2") |> shouldEqual true

module RequestContextTracingTests =
    open System
    open System.Net.Http

    [<Fact>]
    let ``IXRoadRequest has RequestId property`` () =
        let props = typeof<IXRoadRequest>.GetProperties()
        props |> Array.exists (fun p -> p.Name = "RequestId") |> shouldEqual true

    [<Fact>]
    let ``RequestReadyEventArgs has RequestId ServiceCode ServiceVersion`` () =
        let props = typeof<RequestReadyEventArgs>.GetProperties() |> Array.map (fun p -> p.Name)
        props |> Array.contains "RequestId" |> shouldEqual true
        props |> Array.contains "ServiceCode" |> shouldEqual true
        props |> Array.contains "ServiceVersion" |> shouldEqual true

    [<Fact>]
    let ``ResponseReadyEventArgs has RequestId ServiceCode ServiceVersion`` () =
        let props = typeof<ResponseReadyEventArgs>.GetProperties() |> Array.map (fun p -> p.Name)
        props |> Array.contains "RequestId" |> shouldEqual true
        props |> Array.contains "ServiceCode" |> shouldEqual true
        props |> Array.contains "ServiceVersion" |> shouldEqual true

    [<Fact>]
    let ``ResponseReadyEventArgs carries ServiceCode and ServiceVersion`` () =
        let response = { new IXRoadResponse with member _.Save(_) = () }
        let args = ResponseReadyEventArgs(response, XRoadHeader(), "req-id", "listMethods", "v2")
        args.ServiceCode |> shouldEqual "listMethods"
        args.ServiceVersion |> shouldEqual "v2"

    [<Fact>]
    let ``ResponseReadyEventArgs RequestId matches request RequestId for correlation`` () =
        let response = { new IXRoadResponse with member _.Save(_) = () }
        let requestId = Guid.NewGuid().ToString()
        let args = ResponseReadyEventArgs(response, XRoadHeader(), requestId, "svc", "")
        args.RequestId |> shouldEqual requestId

    [<Fact>]
    let ``AbstractEndpointDeclaration exposes RequestReady and ResponseReady events`` () =
        let epType = typeof<AbstractEndpointDeclaration>
        let events = epType.GetEvents() |> Array.map (fun e -> e.Name)
        events |> Array.contains "RequestReady" |> shouldEqual true
        events |> Array.contains "ResponseReady" |> shouldEqual true

    type private TrackingEndpoint() =
        inherit AbstractEndpointDeclaration(Uri("http://test.example.com/"))

    [<Fact>]
    let ``RequestReady event fires exactly once when TriggerRequestReady called once`` () =
        let endpoint = TrackingEndpoint()
        let mutable count = 0
        endpoint.RequestReady.Add(fun _ -> count <- count + 1)
        let request = { new IXRoadRequest with
                            member _.RequestId = "test-id"
                            member _.Save(_: System.IO.Stream) = ()
                            member _.HttpWebRequest = null }
        let args = RequestReadyEventArgs(request, XRoadHeader(), "test-id", "svc", "")
        endpoint.TriggerRequestReady(args)
        count |> shouldEqual 1

    [<Fact>]
    let ``ResponseReady event fires exactly once when TriggerResponseReady called once`` () =
        let endpoint = TrackingEndpoint()
        let mutable count = 0
        endpoint.ResponseReady.Add(fun _ -> count <- count + 1)
        let response = { new IXRoadResponse with member _.Save(_: System.IO.Stream) = () }
        let args = ResponseReadyEventArgs(response, XRoadHeader(), "test-id", "svc", "")
        endpoint.TriggerResponseReady(args)
        count |> shouldEqual 1

    [<Fact>]
    let ``Multiple TriggerRequestReady calls increment counter correctly`` () =
        let endpoint = TrackingEndpoint()
        let mutable count = 0
        endpoint.RequestReady.Add(fun _ -> count <- count + 1)
        let request = { new IXRoadRequest with
                            member _.RequestId = "test-id"
                            member _.Save(_: System.IO.Stream) = ()
                            member _.HttpWebRequest = null }
        endpoint.TriggerRequestReady(RequestReadyEventArgs(request, XRoadHeader(), "id", "svc", ""))
        endpoint.TriggerRequestReady(RequestReadyEventArgs(request, XRoadHeader(), "id", "svc", ""))
        count |> shouldEqual 2
