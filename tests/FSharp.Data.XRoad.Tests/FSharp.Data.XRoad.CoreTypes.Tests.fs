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
