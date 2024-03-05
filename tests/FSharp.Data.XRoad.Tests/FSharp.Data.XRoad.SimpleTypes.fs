module FSharp.Data.XRoad.SimpleTypes

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open FsUnit.Xunit
open FsUnitTyped
open System.Reflection
open Xunit

type SimpleTypes = GenerateTypesFromString<"""
<wsdl:definitions targetNamespace="http://test.x-road.eu/" xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" xmlns:tns="http://test.x-road.eu/">
    <wsdl:types>
        <xs:schema targetNamespace="http://test.x-road.eu/" xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:simpleType name="Age">
                <xs:restriction base="xs:integer">
                    <xs:minInclusive value="0" />
                    <xs:maxInclusive value="100" />
                </xs:restriction>
            </xs:simpleType>
            <xs:simpleType name="Car">
                <xs:restriction base="xs:string">
                    <xs:enumeration value="Audi" />
                    <xs:enumeration value="Volkswagen" />
                    <xs:enumeration value="BMW" />
                </xs:restriction>
            </xs:simpleType>
        </xs:schema>
    </wsdl:types>
</wsdl:definitions>""">

[<Fact>]
let ``Generates simple type without enumeration values`` () =
    let ageType = typeof<SimpleTypes.DefinedTypes.EuXRoadTest.Age>

    let defaultCtor = ageType.GetConstructor(BindingFlags.Instance ||| BindingFlags.NonPublic, null, [||], [||])
    defaultCtor |> should not' (be Null)

    let builderMethod = ageType.GetMethod("Create", BindingFlags.Static ||| BindingFlags.Public, null, [| typeof<bigint> |], [||])
    builderMethod |> should not' (be Null)

    let ctors = ageType.GetConstructors()
    ctors |> shouldBeEmpty

    let baseValueProp = ageType.GetProperty("BaseValue")
    baseValueProp.CanRead |> should be True
    baseValueProp.CanWrite |> should be True
    baseValueProp.SetMethod.IsPrivate |> should be True

    let age = SimpleTypes.DefinedTypes.EuXRoadTest.Age.Create(1000I)
    age.BaseValue |> shouldEqual 1000I

    ageType.GetCustomAttributes() |> shouldHaveLength 1
    ageType |> assertTypeAttribute "Age" "http://test.x-road.eu/" false LayoutKind.Sequence

[<Fact>]
let ``Generates simple type with enumeration values`` () =
    let carType = typeof<SimpleTypes.DefinedTypes.EuXRoadTest.Car>

    let defaultCtor = carType.GetConstructor(BindingFlags.Instance ||| BindingFlags.NonPublic, null, [||], [||])
    defaultCtor |> should not' (be Null)

    let builderMethod = carType.GetMethod("Create", BindingFlags.Static ||| BindingFlags.NonPublic, null, [| typeof<string> |], [||])
    builderMethod |> should not' (be Null)

    let ctors = carType.GetConstructors()
    ctors |> shouldBeEmpty

    let baseValueProp = carType.GetProperty("BaseValue")
    baseValueProp.CanRead |> should be True
    baseValueProp.CanWrite |> should be True
    baseValueProp.SetMethod.IsPrivate |> should be True

    let audiField = carType.GetField("Audi")
    audiField |> should not' (be Null)
    audiField.IsInitOnly |> should be True

    let audi = SimpleTypes.DefinedTypes.EuXRoadTest.Car.Audi
    audi.BaseValue |> shouldEqual "Audi"

    carType.GetCustomAttributes() |> shouldHaveLength 1
    carType |> assertTypeAttribute "Car" "http://test.x-road.eu/" false LayoutKind.Sequence

let [<Literal>] PRODUCER_NAMESPACE = "http://test.x-road.eu/"

[<XRoadType(LayoutKind.Sequence)>]
type HasAge () =
    [<XRoadElement(IsNullable=true)>]
    member val Age: SimpleTypes.DefinedTypes.EuXRoadTest.Age = null with get, set

[<XRoadType(LayoutKind.Sequence)>]
type HasCar () =
    [<XRoadElement(IsNullable=true)>]
    member val Car: SimpleTypes.DefinedTypes.EuXRoadTest.Car = null with get, set

[<Interface>]
type IServices =
    [<XRoadOperation("Service1", "v1", ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service1", PRODUCER_NAMESPACE)>]
    [<XRoadResponse("Service1Response", PRODUCER_NAMESPACE)>]
    abstract Service1: [<XRoadElement>] request: HasAge -> HasAge
    [<XRoadOperation("Service2", "v1", ProtocolVersion = "4.0")>]
    [<XRoadRequest("Service2", PRODUCER_NAMESPACE)>]
    [<XRoadResponse("Service2Response", PRODUCER_NAMESPACE)>]
    abstract Service2: [<XRoadElement>] request: HasCar -> HasCar

let internal serialize = serialize typeof<IServices> PRODUCER_NAMESPACE
let internal serialize' = serialize (SerializerContext())

let internal deserialize = deserialize typeof<IServices>
let deserialize' = deserialize (SerializerContext())

[<Fact>]
let ``can serialize null age`` () =
    let xml = serialize' "Service1" [| HasAge() |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Age xsi:nil="true" /></request></tns:Service1></Body>"""

[<Fact>]
let ``can serialize age value`` () =
    let xml = serialize' "Service1" [| HasAge(Age=SimpleTypes.DefinedTypes.EuXRoadTest.Age.Create(1000I)) |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><request><Age>1000</Age></request></tns:Service1></Body>"""

[<Fact>]
let ``can serialize null car`` () =
    let xml = serialize' "Service2" [| HasCar() |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service2><request><Car xsi:nil="true" /></request></tns:Service2></Body>"""

[<Fact>]
let ``can serialize car value`` () =
    let xml = serialize' "Service2" [| HasCar(Car=SimpleTypes.DefinedTypes.EuXRoadTest.Car.Volkswagen) |> box |]
    xml |> shouldEqual """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service2><request><Car>Volkswagen</Car></request></tns:Service2></Body>"""

[<Fact>]
let ``can deserialize null age values`` () =
    let x =
        """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><Age xsi:nil="true" /></tns:Service1></Body>"""
        |> deserialize' "Service1"
        |> unbox<HasAge>
    x.Age |> should be Null

[<Theory>]
[<InlineData(68)>]
[<InlineData(1000)>]
let ``can deserialize valid age values`` (value: int) =
    let expectedValue = bigint value
    let hd =
        sprintf """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service1><Age>%d</Age></tns:Service1></Body>""" value
        |> deserialize' "Service1"
        |> unbox<HasAge>
    hd.Age.BaseValue |> shouldEqual expectedValue

[<Fact>]
let ``can deserialize null car values`` () =
    let x =
        """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service2><Car xsi:nil="true" /></tns:Service2></Body>"""
        |> deserialize' "Service2"
        |> unbox<HasCar>
    x.Car |> should be Null

[<Theory>]
[<InlineData("Audi")>]
[<InlineData("BMW")>]
[<InlineData("Volkswagen")>]
[<InlineData("Mazda")>]
let ``can deserialize valid car values`` (value: string) =
    let hd =
        sprintf """<?xml version="1.0" encoding="utf-8"?><Body xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:tns="http://test.x-road.eu/" xmlns:test="testns"><tns:Service2><Car>%s</Car></tns:Service2></Body>""" value
        |> deserialize' "Service2"
        |> unbox<HasCar>
    hd.Car.BaseValue |> shouldEqual value
