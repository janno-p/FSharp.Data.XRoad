module FSharp.Data.XRoad.SimpleTypes

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open NUnit.Framework
open System.Reflection

type SimpleTypes = GenerateTypesFromString<"""
<wsdl:definitions targetNamespace="http://test.x-road.eu/" xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" xmlns:tns="http://test.x-road.eu">
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

[<Test>]
let ``Generates simple type without enumeration values`` () =
    let ageType = typeof<SimpleTypes.DefinedTypes.test.Age>

    let defaultCtor = ageType.GetConstructor(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public, null, [||], [||])
    Assert.IsNull(defaultCtor, "No default constructor should be defined")

    let valueCtor = ageType.GetConstructor(BindingFlags.Instance ||| BindingFlags.Public, null, [| typeof<bigint> |], [||])
    Assert.IsNotNull(valueCtor, "Value initializer constructor should be public")

    let baseValueProp = ageType.GetProperty("BaseValue")
    Assert.IsTrue(baseValueProp.CanRead, "BaseValue property should be readable")
    Assert.IsFalse(baseValueProp.CanWrite, "BaseValue property should not be writeable")

    let age = SimpleTypes.DefinedTypes.test.Age(1000I)
    Assert.AreEqual(1000I, age.BaseValue)

    let attributes = ageType.GetCustomAttributes() |> Seq.toList
    Assert.AreEqual(1, attributes.Length)

    match attributes |> Seq.tryFind (fun a -> a :? XRoadTypeAttribute) with
    | Some(attr) ->
        let attr = attr :?> XRoadTypeAttribute
        Assert.IsFalse(attr.IsAnonymous)
        Assert.AreEqual(LayoutKind.Sequence, attr.Layout)
        Assert.AreEqual("Age", attr.Name)
        Assert.AreEqual("http://test.x-road.eu/", attr.Namespace)
    | None -> Assert.Fail("Age type should have XRoadTypeAttribute defined.")

[<Test>]
let ``Generates simple type with enumeration values`` () =
    let carType = typeof<SimpleTypes.DefinedTypes.test.Car>

    let defaultCtor = carType.GetConstructor(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public, null, [||], [||])
    Assert.IsNull(defaultCtor, "No default constructor should be defined")

    let valueCtor = carType.GetConstructor(BindingFlags.Instance ||| BindingFlags.NonPublic, null, [| typeof<string> |], [||])
    Assert.IsNotNull(valueCtor, "Value initializer constructor should be private")

    let ctors = carType.GetConstructors()
    Assert.AreEqual(0, ctors.Length, "No public constructors should be available for this type")

    let baseValueProp = carType.GetProperty("BaseValue")
    Assert.IsTrue(baseValueProp.CanRead, "BaseValue property should be readable")
    Assert.IsFalse(baseValueProp.CanWrite, "BaseValue property should not be writeable")

    let audiField = carType.GetField("Audi")
    Assert.IsNotNull(audiField, "Audi field should be public")
    Assert.IsTrue(audiField.IsInitOnly, "Audi field should be read-only")

    let audi = SimpleTypes.DefinedTypes.test.Car.Audi
    Assert.AreEqual("Audi", audi.BaseValue, "Audi field should have enumeration name assigned to BaseValue")

    let attributes = carType.GetCustomAttributes() |> Seq.toList
    Assert.AreEqual(1, attributes.Length)

    match attributes |> Seq.tryFind (fun a -> a :? XRoadTypeAttribute) with
    | Some(attr) ->
        let attr = attr :?> XRoadTypeAttribute
        Assert.IsFalse(attr.IsAnonymous)
        Assert.AreEqual(LayoutKind.Sequence, attr.Layout)
        Assert.AreEqual("Car", attr.Name)
        Assert.AreEqual("http://test.x-road.eu/", attr.Namespace)
    | None -> Assert.Fail("Car type should have XRoadTypeAttribute defined.")
