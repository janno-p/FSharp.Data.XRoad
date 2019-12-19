module FSharp.Data.XRoad.ComplexTypes

open FSharp.Data.XRoad
open NUnit.Framework
open System.Reflection

type ComplexTypes = GenerateTypesFromString<"""
<wsdl:definitions targetNamespace="http://test.x-road.eu/" xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" xmlns:tns="http://test.x-road.eu">
    <wsdl:types>
        <xs:schema targetNamespace="http://test.x-road.eu/" xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:complexType name="AbstractType" abstract="true">
                <xs:sequence />
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
