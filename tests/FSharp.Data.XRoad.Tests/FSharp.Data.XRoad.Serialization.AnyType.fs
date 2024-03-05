module FSharp.Data.XRoad.Serialization.AnyType

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open FsUnit.Xunit
open FsUnitTyped
open System.Xml.Linq
open Xunit

let [<Literal>] PRODUCER_NAMESPACE = "http://test.x-road.eu/"

[<XRoadType(LayoutKind.Sequence)>]
type HasAnyType () =
    [<XRoadElement(IsNullable=true)>]
    member val AnyType = Unchecked.defaultof<XElement> with get, set

[<Interface>]
type IServices =
    [<XRoadOperation("Service1", "v1", ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service1", PRODUCER_NAMESPACE)>]
    [<XRoadResponse("Service1Response", PRODUCER_NAMESPACE)>]
    abstract Service1: [<XRoadElement>] request: HasAnyType -> HasAnyType

let internal serialize = serialize typeof<IServices> PRODUCER_NAMESPACE
let internal serialize' = serialize (SerializerContext())

let internal deserialize = deserialize typeof<IServices>
let deserialize' = deserialize (SerializerContext())

[<Fact>]
let ``can serialize null date`` () =
    let xml = serialize' "Service1" [| HasAnyType() |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><AnyType xsi:nil="true" /></request></tns:Service1></Body>"""

[<Fact>]
let ``can serialize any value`` () =
    let anyType = XElement(XName.Get("Jura"), XAttribute(XName.Get("random"), "4"), "Content")
    let xml = serialize' "Service1" [| HasAnyType(AnyType=anyType) |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><AnyType random="4">Content</AnyType></request></tns:Service1></Body>"""

[<Fact>]
let ``can deserialize nil content`` () =
    let anyType =
        """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><AnyType xsi:nil="true" /></tns:Service1></Body>"""
        |> deserialize' "Service1"
        |> unbox<HasAnyType>
    anyType.AnyType |> should not' (be Null)
    anyType.AnyType.Value |> should be EmptyString
    let nodes = anyType.AnyType.Nodes() |> Seq.toList
    nodes |> shouldBeEmpty
    let attributes = anyType.AnyType.Attributes() |> Seq.toList
    attributes |> shouldHaveLength 1
    attributes[0].Name |> shouldEqual (XName.Get("nil", XmlNamespace.Xsi))
    attributes[0].Value |> shouldEqual "true"

[<Fact>]
let ``can deserialize any content`` () =
    let anyType =
        """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><AnyType random="4">Content</AnyType></tns:Service1></Body>"""
        |> deserialize' "Service1"
        |> unbox<HasAnyType>
    anyType.AnyType |> should not' (be Null)
    let nodes = anyType.AnyType.Nodes() |> Seq.toList
    nodes |> shouldHaveLength 1
    nodes[0] |> should be instanceOfType<XText>
    let text = nodes[0] :?> XText
    text.Value |> shouldEqual "Content"
    let attributes = anyType.AnyType.Attributes() |> Seq.toList
    attributes |> shouldHaveLength 1
    attributes[0].Name |> shouldEqual (XName.Get("random"))
    attributes[0].Value |> shouldEqual "4"
