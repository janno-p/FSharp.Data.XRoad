module FSharp.Data.XRoad.Serialization.Date

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open FsUnitTyped
open NodaTime
open NodaTime.Text
open System
open Xunit

let [<Literal>] PRODUCER_NAMESPACE = "http://test.x-road.eu/"

[<XRoadType(LayoutKind.Sequence)>]
type HasDate () =
    [<XRoadElement(IsNullable=true)>]
    member val Date = Nullable<OffsetDate>() with get, set

[<Interface>]
type IServices =
    [<XRoadOperation("Service1", "v1", ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service1", PRODUCER_NAMESPACE)>]
    [<XRoadResponse("Service1Response", PRODUCER_NAMESPACE)>]
    abstract Service1: [<XRoadElement>] request: HasDate -> HasDate

let internal serialize = serialize typeof<IServices> PRODUCER_NAMESPACE
let internal serialize' = serialize (SerializerContext(DefaultOffset=Offset.FromHours(2)))

let internal deserialize = deserialize typeof<IServices>
let deserialize' = deserialize (SerializerContext(DefaultOffset=Offset.FromHours(2)))

[<Fact>]
let ``can serialize null date`` () =
    let xml = serialize' "Service1" [| HasDate() |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Date xsi:nil="true" /></request></tns:Service1></Body>"""

[<Fact>]
let ``can serialize date value`` () =
    let date = OffsetDate(LocalDate(2019, 12, 26), Offset.Zero)
    let xml = serialize' "Service1" [| HasDate(Date=Nullable(date)) |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Date>2019-12-26Z</Date></request></tns:Service1></Body>"""

[<Fact>]
let ``can serialize date value with offset`` () =
    let date = OffsetDate(LocalDate(2019, 12, 26), Offset.FromHours(3))
    let xml = serialize' "Service1" [| HasDate(Date=Nullable(date)) |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Date>2019-12-26+03:00</Date></request></tns:Service1></Body>"""

[<Theory>]
[<InlineData("2004-04-12", "2004-04-12+02")>]
[<InlineData("-0045-01-01", "-0045-01-01+02")>]
//[<InlineData("12004-04-12", "12004-04-12+02")>]
[<InlineData("2004-04-12-05:00", "2004-04-12-05")>]
[<InlineData("2004-04-12Z", "2004-04-12+00")>]
let ``can deserialize valid date values`` (value: string, expectedValue: string) =
    let expectedValue = OffsetDatePattern.GeneralIso.Parse(expectedValue).GetValueOrThrow()
    let hd =
        sprintf """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><Date>%s</Date></tns:Service1></Body>""" value
        |> deserialize' "Service1"
        |> unbox<HasDate>
    hd.Date.Value |> shouldEqual expectedValue

[<Theory>]
[<InlineData("99-04-12", "The value string does not match the required number from the format string \"uuuu\". Value being parsed: '^99-04-12'. (^ indicates error position.)")>]
[<InlineData("2004-4-2", "The value string does not match the required number from the format string \"MM\". Value being parsed: '2004-^4-2'. (^ indicates error position.)")>]
[<InlineData("2004/04/02", "The value string does not match a quoted string in the pattern. Value being parsed: '2004^/04/02'. (^ indicates error position.)")>]
[<InlineData("04-12-2004", "The value string does not match the required number from the format string \"uuuu\". Value being parsed: '^04-12-2004'. (^ indicates error position.)")>]
[<InlineData("2004-04-31", "The required value sign is missing. Value being parsed: '2004-04-31^'. (^ indicates error position.)")>]
let ``errors on invalid date values`` (value: string, expectedMessage: string) =
    let ex =
        Assert.Throws<UnparsableValueException>(fun _ ->
            sprintf """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><Date>%s</Date></tns:Service1></Body>""" value
            |> deserialize' "Service1"
            |> ignore
        )
    ex.Message |> shouldEqual expectedMessage
