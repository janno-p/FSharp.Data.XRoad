module FSharp.Data.XRoad.Serialization.Time

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open NodaTime
open NodaTime.Text
open NUnit.Framework
open System

let [<Literal>] PRODUCER_NAMESPACE = "http://test.x-road.eu/"

[<XRoadType(LayoutKind.Sequence)>]
type HasTime () =
    [<XRoadElement(IsNullable=true)>]
    member val Time = Nullable<OffsetTime>() with get, set

[<Interface>]
type IServices =
    [<XRoadOperation("Service1", "v1", ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service1", PRODUCER_NAMESPACE)>]
    [<XRoadResponse("Service1Response", PRODUCER_NAMESPACE)>]
    abstract Service1: [<XRoadElement>] request: HasTime -> HasTime

let internal serialize = serialize typeof<IServices> PRODUCER_NAMESPACE
let internal serialize' = serialize (SerializerContext(DefaultOffset=Offset.FromHours(2)))

let internal deserialize = deserialize typeof<IServices>
let deserialize' = deserialize (SerializerContext(DefaultOffset=Offset.FromHours(2)))

[<Test>]
let ``can serialize null time`` () =
    let xml = serialize' "Service1" [| HasTime() |> box |]
    Assert.AreEqual(xml, """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Time xsi:nil="true" /></request></tns:Service1></Body>""")

[<Test>]
let ``can serialize time value`` () =
    let time = OffsetTime(LocalTime(10, 11, 12), Offset.Zero)
    let xml = serialize' "Service1" [| HasTime(Time=Nullable(time)) |> box |]
    Assert.AreEqual(xml, """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Time>10:11:12Z</Time></request></tns:Service1></Body>""")

[<Test>]
let ``can serialize time value with offset`` () =
    let time = OffsetTime(LocalTime(10, 11, 12), Offset.FromHours(3))
    let xml = serialize' "Service1" [| HasTime(Time=Nullable(time)) |> box |]
    Assert.AreEqual(xml, """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Time>10:11:12+03:00</Time></request></tns:Service1></Body>""")

[<Test>]
let ``can serialize time value with fractions and offset`` () =
    let time = OffsetTime(LocalTime(10, 11, 12, 334), Offset.FromHours(3))
    let xml = serialize' "Service1" [| HasTime(Time=Nullable(time)) |> box |]
    Assert.AreEqual(xml, """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Time>10:11:12.334+03:00</Time></request></tns:Service1></Body>""")

[<TestCase("13:20:00", "13:20:00+02")>]
[<TestCase("13:20:30.5555", "13:20:30.5555+02")>]
[<TestCase("13:20:00-05:00", "13:20:00-05")>]
[<TestCase("13:20:00Z", "13:20:00Z")>]
[<TestCase("00:00:00", "00:00:00+02")>]
//[<TestCase("24:00:00", "24:00:00")>]
let ``can deserialize valid time values`` (value: string, expectedValue: string) =
    let expectedValue = OffsetTimePattern.ExtendedIso.Parse(expectedValue).GetValueOrThrow()
    let hd =
        sprintf """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><Time>%s</Time></tns:Service1></Body>""" value
        |> deserialize' "Service1"
        |> unbox<HasTime>
    Assert.AreEqual(expectedValue, hd.Time)

[<TestCase("5:20:00", "The value string does not match the required number from the format string \"HH\". Value being parsed: '^5:20:00'. (^ indicates error position.)")>]
[<TestCase("13:20", "The value string does not match a quoted string in the pattern. Value being parsed: '13:20^'. (^ indicates error position.)")>]
[<TestCase("13:20.5:00", "The value string does not match a quoted string in the pattern. Value being parsed: '13:20^.5:00'. (^ indicates error position.)")>]
[<TestCase("13:65:00", "The value 65 is out of range for the field 'm' in the NodaTime.OffsetTime type. Value being parsed: '13:^65:00'. (^ indicates error position.)")>]
[<TestCase("", "The value string is empty.")>]
let ``errors on invalid time values`` (value: string, expectedMessage: string) =
    let ex =
        Assert.Throws<UnparsableValueException>(fun _ ->
            sprintf """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><Time>%s</Time></tns:Service1></Body>""" value
            |> deserialize' "Service1"
            |> ignore
        )
    Assert.AreEqual(expectedMessage, ex.Message)
