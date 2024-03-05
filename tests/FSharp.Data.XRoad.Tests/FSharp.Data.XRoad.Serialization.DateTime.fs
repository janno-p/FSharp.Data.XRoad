module FSharp.Data.XRoad.Serialization.DateTime

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open FsUnitTyped
open NodaTime
open NodaTime.Text
open System
open Xunit

let [<Literal>] PRODUCER_NAMESPACE = "http://test.x-road.eu/"

[<XRoadType(LayoutKind.Sequence)>]
type HasDateTime () =
    [<XRoadElement(IsNullable=true)>]
    member val DateTime = Nullable<OffsetDateTime>() with get, set

[<Interface>]
type IServices =
    [<XRoadOperation("Service1", "v1", ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service1", PRODUCER_NAMESPACE)>]
    [<XRoadResponse("Service1Response", PRODUCER_NAMESPACE)>]
    abstract Service1: [<XRoadElement>] request: HasDateTime -> HasDateTime

let internal serialize = serialize typeof<IServices> PRODUCER_NAMESPACE
let internal serialize' = serialize (SerializerContext(DefaultOffset=Offset.FromHours(2)))

let internal deserialize = deserialize typeof<IServices>
let deserialize' = deserialize (SerializerContext(DefaultOffset=Offset.FromHours(2)))

[<Fact>]
let ``can serialize null dateTime`` () =
    let xml = serialize' "Service1" [| HasDateTime() |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><DateTime xsi:nil="true" /></request></tns:Service1></Body>"""

[<Fact>]
let ``can serialize dateTime value`` () =
    let dateTime = OffsetDateTime(LocalDateTime(2019, 12, 26, 10, 11, 12), Offset.Zero)
    let xml = serialize' "Service1" [| HasDateTime(DateTime=Nullable(dateTime)) |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><DateTime>2019-12-26T10:11:12Z</DateTime></request></tns:Service1></Body>"""

[<Fact>]
let ``can serialize dateTime value with offset`` () =
    let dateTime = OffsetDateTime(LocalDateTime(2019, 12, 26, 10, 11, 12), Offset.FromHours(3))
    let xml = serialize' "Service1" [| HasDateTime(DateTime=Nullable(dateTime)) |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><DateTime>2019-12-26T10:11:12+03:00</DateTime></request></tns:Service1></Body>"""

[<Fact>]
let ``can serialize dateTime value with fractions and offset`` () =
    let dateTime = OffsetDateTime(LocalDateTime(2019, 12, 26, 10, 11, 12, 334), Offset.FromHours(3))
    let xml = serialize' "Service1" [| HasDateTime(DateTime=Nullable(dateTime)) |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><DateTime>2019-12-26T10:11:12.334+03:00</DateTime></request></tns:Service1></Body>"""

[<Theory>]
[<InlineData("2004-04-12T13:20:00", "2004-04-12T13:20:00+02")>]
[<InlineData("2004-04-12T13:20:15.5", "2004-04-12T13:20:15.5+02")>]
[<InlineData("2004-04-12T13:20:00-05:00", "2004-04-12T13:20:00-05")>]
[<InlineData("2004-04-12T13:20:00Z", "2004-04-12T13:20:00Z")>]
let ``can deserialize valid dateTime values`` (value: string, expectedValue: string) =
    let expectedValue = OffsetDateTimePattern.ExtendedIso.Parse(expectedValue).GetValueOrThrow()
    let hd =
        sprintf """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><DateTime>%s</DateTime></tns:Service1></Body>""" value
        |> deserialize' "Service1"
        |> unbox<HasDateTime>
    hd.DateTime.Value |> shouldEqual expectedValue

[<Theory>]
[<InlineData("2004-04-12T13:00", "The value string does not match a quoted string in the pattern. Value being parsed: '2004-04-12T13:00^'. (^ indicates error position.)")>]
[<InlineData("2004-04-1213:20:00", "The value string does not match a quoted string in the pattern. Value being parsed: '2004-04-12^13:20:00'. (^ indicates error position.)")>]
[<InlineData("99-04-12T13:00", "The value string does not match the required number from the format string \"uuuu\". Value being parsed: '^99-04-12T13:00'. (^ indicates error position.)")>]
[<InlineData("2004-04-12", "The value string does not match a quoted string in the pattern. Value being parsed: '2004-04-12^'. (^ indicates error position.)")>]
let ``errors on invalid dateTime values`` (value: string, expectedMessage: string) =
    let ex =
        Assert.Throws<UnparsableValueException>(fun _ ->
            sprintf """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><DateTime>%s</DateTime></tns:Service1></Body>""" value
            |> deserialize' "Service1"
            |> ignore
        )
    ex.Message |> shouldEqual expectedMessage
