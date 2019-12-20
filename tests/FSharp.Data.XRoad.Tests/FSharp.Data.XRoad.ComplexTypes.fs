module FSharp.Data.XRoad.ComplexTypes

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open NUnit.Framework
open System.Reflection

type ComplexTypes = GenerateTypesFromString<"""
<wsdl:definitions targetNamespace="http://test.x-road.eu/" xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" xmlns:tns="http://test.x-road.eu/">
    <wsdl:types>
        <xs:schema targetNamespace="http://test.x-road.eu/" xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:complexType name="AbstractType" abstract="true">
                <xs:sequence />
            </xs:complexType>
            <xs:complexType name="DerivedType">
                <xs:complexContent>
                    <xs:extension base="tns:AbstractType">
                    </xs:extension>
                </xs:complexContent>
            </xs:complexType>
            <xs:complexType name="ShoeSize">
                <xs:simpleContent>
                    <xs:extension base="xs:integer">
                        <xs:attribute name="country" type="xs:string" />
                    </xs:extension>
                </xs:simpleContent>
            </xs:complexType>
        </xs:schema>
    </wsdl:types>
</wsdl:definitions>""">

[<Test>]
let ``generates correct abstract base type`` () =
    let typ = typeof<ComplexTypes.DefinedTypes.test.AbstractType>
    Assert.IsTrue(typ.IsAbstract, "Abstract type should be define as abstract.")

    let defaultCtor = typ.GetConstructor(BindingFlags.NonPublic ||| BindingFlags.Instance, null, [||], [||])
    Assert.IsNotNull(defaultCtor, "Abstract type should have protected default constructor.")
    Assert.IsTrue(defaultCtor.IsFamily, "Abstract type should have protected default constructor.")

    Assert.AreEqual(1, typ.GetCustomAttributes() |> Seq.length)
    typ |> assertTypeAttribute "AbstractType" "http://test.x-road.eu/" false LayoutKind.Sequence

[<Test>]
let ``generates correct simpleContent type`` () =
    let typ = typeof<ComplexTypes.DefinedTypes.test.ShoeSize>
    Assert.IsFalse(typ.IsAbstract, "ShoeSize type should not be define as abstract.")

    let defaultCtor = typ.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [||], [||])
    Assert.IsNotNull(defaultCtor, "ShoeSize type should have public default constructor.")

    let baseValueProp = typ.GetProperty("BaseValue")
    Assert.IsNotNull(baseValueProp, "ShoeSize type should have BaseValue property")
    Assert.IsTrue(baseValueProp.CanRead, "BaseValue property should be readable")
    Assert.IsTrue(baseValueProp.CanWrite, "BaseValue property should be writeable")
    Assert.AreEqual(typeof<bigint>, baseValueProp.PropertyType)

    let shoeSize = ComplexTypes.DefinedTypes.test.ShoeSize()
    shoeSize.BaseValue <- 1234I
    Assert.AreEqual(1234I, shoeSize.BaseValue)

    Assert.AreEqual(1, typ.GetCustomAttributes() |> Seq.length)
    typ |> assertTypeAttribute "ShoeSize" "http://test.x-road.eu/" false LayoutKind.Sequence

[<Test>]
let ``generates correct derived type`` () =
    let typ = typeof<ComplexTypes.DefinedTypes.test.DerivedType>
    Assert.IsFalse(typ.IsAbstract, "DerivedType type should not be define as abstract.")

    let baseTy = typ.BaseType
    Assert.AreEqual(baseTy, typeof<ComplexTypes.DefinedTypes.test.AbstractType>, "DerivedType should be based on AbstractType")

    let defaultCtor = typ.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [||], [||])
    Assert.IsNotNull(defaultCtor, "DerivedType type should have public default constructor.")

    Assert.AreEqual(1, typ.GetCustomAttributes() |> Seq.length)
    typ |> assertTypeAttribute "DerivedType" "http://test.x-road.eu/" false LayoutKind.Sequence

    let derivedTypeInstance = ComplexTypes.DefinedTypes.test.DerivedType()
    Assert.IsNotNull(derivedTypeInstance)
    Assert.IsInstanceOf<ComplexTypes.DefinedTypes.test.AbstractType>(derivedTypeInstance)
