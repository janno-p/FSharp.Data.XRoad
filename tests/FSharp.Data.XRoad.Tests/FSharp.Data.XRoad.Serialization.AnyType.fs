module FSharp.Data.XRoad.Serialization.AnyType

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open NUnit.Framework
open System.Xml.Linq

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

[<Test>]
let ``can serialize null date`` () =
    let xml = serialize' "Service1" [| HasAnyType() |> box |]
    Assert.AreEqual(xml, """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><AnyType xsi:nil="true" /></request></tns:Service1></Body>""")

[<Test>]
let ``can serialize any value`` () =
    let anyType = XElement(XName.Get("Jura"), XAttribute(XName.Get("random"), "4"), "Content")
    let xml = serialize' "Service1" [| HasAnyType(AnyType=anyType) |> box |]
    Assert.AreEqual(xml, """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><AnyType random="4">Content</AnyType></request></tns:Service1></Body>""")

[<Test>]
let ``can deserialize nil content`` () =
    let anyType =
        """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><AnyType xsi:nil="true" /></tns:Service1></Body>"""
        |> deserialize' "Service1"
        |> unbox<HasAnyType>
    Assert.IsNotNull(anyType.AnyType)
    Assert.AreEqual("", anyType.AnyType.Value)
    let nodes = anyType.AnyType.Nodes() |> Seq.toList
    Assert.AreEqual(0, nodes.Length)
    let attributes = anyType.AnyType.Attributes() |> Seq.toList
    Assert.AreEqual(1, attributes.Length)
    Assert.AreEqual(XName.Get("nil", XmlNamespace.Xsi), attributes[0].Name)
    Assert.AreEqual("true", attributes[0].Value)

[<Test>]
let ``can deserialize any content`` () =
    let anyType =
        """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><AnyType random="4">Content</AnyType></tns:Service1></Body>"""
        |> deserialize' "Service1"
        |> unbox<HasAnyType>
    Assert.IsNotNull(anyType.AnyType)
    let nodes = anyType.AnyType.Nodes() |> Seq.toList
    Assert.AreEqual(1, nodes.Length)
    Assert.IsInstanceOf<XText>(nodes[0])
    let text = nodes[0] :?> XText
    Assert.AreEqual("Content", text.Value)
    let attributes = anyType.AnyType.Attributes() |> Seq.toList
    Assert.AreEqual(1, attributes.Length)
    Assert.AreEqual(XName.Get("random"), attributes[0].Name)
    Assert.AreEqual("4", attributes[0].Value)
