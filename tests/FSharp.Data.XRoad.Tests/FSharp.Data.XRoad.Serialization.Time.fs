module FSharp.Data.XRoad.Serialization.Time

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open FsUnitTyped
open NodaTime
open NodaTime.Text
open System
open Xunit

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

[<Fact>]
let ``can serialize null time`` () =
    let xml = serialize' "Service1" [| HasTime() |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Time xsi:nil="true" /></request></tns:Service1></Body>"""

[<Fact>]
let ``can serialize time value`` () =
    let time = OffsetTime(LocalTime(10, 11, 12), Offset.Zero)
    let xml = serialize' "Service1" [| HasTime(Time=Nullable(time)) |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Time>10:11:12Z</Time></request></tns:Service1></Body>"""

[<Fact>]
let ``can serialize time value with offset`` () =
    let time = OffsetTime(LocalTime(10, 11, 12), Offset.FromHours(3))
    let xml = serialize' "Service1" [| HasTime(Time=Nullable(time)) |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Time>10:11:12+03:00</Time></request></tns:Service1></Body>"""

[<Fact>]
let ``can serialize time value with fractions and offset`` () =
    let time = OffsetTime(LocalTime(10, 11, 12, 334), Offset.FromHours(3))
    let xml = serialize' "Service1" [| HasTime(Time=Nullable(time)) |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Time>10:11:12.334+03:00</Time></request></tns:Service1></Body>"""

[<Theory>]
[<InlineData("13:20:00", "13:20:00+02")>]
[<InlineData("13:20:30.5555", "13:20:30.5555+02")>]
[<InlineData("13:20:00-05:00", "13:20:00-05")>]
[<InlineData("13:20:00Z", "13:20:00Z")>]
[<InlineData("00:00:00", "00:00:00+02")>]
//[<InlineData("24:00:00", "24:00:00")>]
let ``can deserialize valid time values`` (value: string, expectedValue: string) =
    let expectedValue = OffsetTimePattern.ExtendedIso.Parse(expectedValue).GetValueOrThrow()
    let hd =
        sprintf """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><Time>%s</Time></tns:Service1></Body>""" value
        |> deserialize' "Service1"
        |> unbox<HasTime>
    hd.Time.Value |> shouldEqual expectedValue

[<Theory>]
[<InlineData("5:20:00", "The value string does not match the required number from the format string \"HH\". Value being parsed: '^5:20:00'. (^ indicates error position.)")>]
[<InlineData("13:20", "The value string does not match a quoted string in the pattern. Value being parsed: '13:20^'. (^ indicates error position.)")>]
[<InlineData("13:20.5:00", "The value string does not match a quoted string in the pattern. Value being parsed: '13:20^.5:00'. (^ indicates error position.)")>]
[<InlineData("13:65:00", "The value 65 is out of range for the field 'm' in the NodaTime.OffsetTime type. Value being parsed: '13:^65:00'. (^ indicates error position.)")>]
[<InlineData("", "The value string is empty.")>]
let ``errors on invalid time values`` (value: string, expectedMessage: string) =
    let ex =
        Assert.Throws<UnparsableValueException>(fun _ ->
            sprintf """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><Time>%s</Time></tns:Service1></Body>""" value
            |> deserialize' "Service1"
            |> ignore
        )
    ex.Message |> shouldEqual expectedMessage
