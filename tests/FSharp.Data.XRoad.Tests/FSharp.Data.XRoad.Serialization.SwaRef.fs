module FSharp.Data.XRoad.Serialization.SwaRef

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open NUnit.Framework
open System.Text
open Optional.Unsafe

let assertEquals expected actual = Assert.AreEqual(expected, actual)
let assertIsNull actual = Assert.IsNull(actual)
let assertSame expected actual = Assert.AreSame(expected, actual)

let [<Literal>] PRODUCER_NAMESPACE = "http://test.x-road.eu/"

[<XRoadType(LayoutKind.Sequence)>]
type HasSwaRef () =
    [<XRoadElement(IsNullable=true, TypeHint = TypeHint.SwaRef)>]
    member val Datafile = Unchecked.defaultof<BinaryContent> with get, set

[<XRoadType("ProcessingStatus", LayoutKind.Sequence, Namespace = "http://stat-v6.x-road.eu")>]
type ProcessingStatus() =
    [<XRoadElement(-1, "", MergeContent = true, TypeHint = TypeHint.Token)>]
    member val BaseValue = Unchecked.defaultof<string> with get, set
    static member val _0 = ProcessingStatus(BaseValue = "0")
    static member val _1 = ProcessingStatus(BaseValue = "1")

[<XRoadType("ValidationResult", LayoutKind.Sequence, Namespace="http://stat-v6.x-road.eu")>]
type ValidationResult() =
    [<XRoadElement(-1, "", MergeContent = true, TypeHint = TypeHint.Token)>]
    member val BaseValue = Unchecked.defaultof<string> with get, set
    static member val _0 = ValidationResult(BaseValue = "0")
    static member val _1 = ValidationResult(BaseValue = "1")
    static member val _2 = ValidationResult(BaseValue = "2")

[<XRoadType("ReturnErrorResponse", LayoutKind.Sequence, Namespace="http://stat-v6.x-road.eu")>]
type ReturnErrorResponse() =
    [<XRoadElement(-1, "")>]
    member val ProcessingStatus = Unchecked.defaultof<ProcessingStatus> with get, set
    [<XRoadElement(-1, "")>]
    member val ValidationResult = Optional.Option.None<ValidationResult>() with get, set
    [<XRoadElement(-1, "", TypeHint = TypeHint.SwaRef)>]
    member val DataFile = Optional.Option.None<BinaryContent>() with get, set
    [<XRoadElement(-1, "", TypeHint = TypeHint.String)>]
    member val errorMessage = Optional.Option.None<string>() with get, set

[<Interface>]
type IServices =
    [<XRoadOperation("Service1", "v1", ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service1", PRODUCER_NAMESPACE)>]
    [<XRoadResponse("Service1Response", PRODUCER_NAMESPACE)>]
    abstract Service1: [<XRoadElement>] request: HasSwaRef -> HasSwaRef
    [<XRoadOperation("Service2", "v1", ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service2", PRODUCER_NAMESPACE)>]
    [<XRoadResponse("Service2Response", PRODUCER_NAMESPACE)>]
    abstract Service2: [<XRoadElement>] response: ReturnErrorResponse -> ReturnErrorResponse

let internal serialize = serialize typeof<IServices> PRODUCER_NAMESPACE
let internal serialize' = serialize (SerializerContext())

let testAttachment = BinaryContent.Create(Encoding.UTF8.GetBytes("test"))

let internal deserializerContext =
    let ctx = SerializerContext()
    ctx.Attachments.Add("test-id", testAttachment)
    ctx

let internal deserialize = deserialize typeof<IServices>
let internal deserialize' = deserialize deserializerContext

[<Test>]
let ``can serialize null swaRef content`` () =
    serialize' "Service1" [| HasSwaRef() |> box |]
    |> assertEquals """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Datafile xsi:nil="true" /></request></tns:Service1></Body>"""

[<Test>]
let ``can serialize swaRef content`` () =
    let content = Encoding.UTF8.GetBytes("test")
    let datafile = BinaryContent.Create("test-id", content)
    serialize' "Service1" [| HasSwaRef(Datafile = datafile) |> box |]
    |> assertEquals """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Datafile>cid:test-id</Datafile></request></tns:Service1></Body>"""

[<Test>]
let ``can deserialize null swaRef content`` () =
    let hsr =
        """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><Datafile xsi:nil="true" /></tns:Service1></Body>"""
        |> deserialize' "Service1"
        |> unbox<HasSwaRef>
    hsr.Datafile |> assertIsNull

[<Test>]
let ``can deserialize swaRef content`` () =
    let hsr =
        """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><Datafile>cid:test-id</Datafile></tns:Service1></Body>"""
        |> deserialize' "Service1"
        |> unbox<HasSwaRef>
    hsr.Datafile |> assertSame testAttachment

[<Test>]
let ``can deserialize empty swaRef content`` () =
    let hsr =
        """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><Datafile></Datafile></tns:Service1></Body>"""
        |> deserialize' "Service1"
        |> unbox<HasSwaRef>
    Assert.IsNotNull(hsr.Datafile)
    Assert.AreEqual(0, hsr.Datafile.GetBytes().Length)

[<Test>]
let ``deserialize estat response`` () =
    let response =
        """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service2><ProcessingStatus></ProcessingStatus><ValidationResult></ValidationResult><DataFile>cid:test-id</DataFile><errorMessage>Could not find validation report on ticket 8f7758ac-cc32-4965-ab50-85e3d1f411df</errorMessage></tns:Service2></Body>"""
        |> deserialize' "Service2"
        |> unbox<ReturnErrorResponse>
    Assert.IsNotNull(response)
    Assert.IsTrue(response.DataFile.HasValue)
    Assert.AreEqual(4, response.DataFile.ValueOrFailure().GetBytes().Length)
    Assert.AreEqual("", response.ProcessingStatus.BaseValue)
    Assert.IsTrue(response.ValidationResult.HasValue)
    Assert.AreEqual("", response.ValidationResult.ValueOrFailure().BaseValue)
    Assert.IsTrue(response.errorMessage.HasValue)
    Assert.AreEqual("Could not find validation report on ticket 8f7758ac-cc32-4965-ab50-85e3d1f411df", response.errorMessage.ValueOrFailure())
