module FSharp.Data.XRoad.Elements

open FSharp.Data.XRoad
open NUnit.Framework
open System.Xml.Linq

type Elements = GenerateTypesFromString<"""
<wsdl:definitions targetNamespace="http://test.x-road.eu/" xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" xmlns:tns="http://test.x-road.eu/">
    <wsdl:types>
        <xs:schema targetNamespace="http://test.x-road.eu/" xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:complexType name="HasElements">
                <xs:sequence>
                    <xs:element name="HasAnyType" />
                    <xs:element name="AnyTypeWithAnnotations">
                        <xs:annotation />
                    </xs:element>
                    <xs:element name="HasEmptyDefinition">
                        <xs:complexType>
                            <xs:sequence />
                        </xs:complexType>
                    </xs:element>
                </xs:sequence>
            </xs:complexType>
        </xs:schema>
    </wsdl:types>
</wsdl:definitions>""">

[<Test>]
let ``has correct type definitions for elements`` () =
    let rootType = typeof<Elements.DefinedTypes.EuXRoadTest.HasElements>
    let hasAnyTypeProp = rootType.GetProperty("HasAnyType")
    Assert.IsNotNull(hasAnyTypeProp)
    Assert.AreEqual(typeof<XElement>, hasAnyTypeProp.PropertyType)
    let anyTypeWithAnnotationsProp = rootType.GetProperty("AnyTypeWithAnnotations")
    Assert.IsNotNull(anyTypeWithAnnotationsProp)
    Assert.AreEqual(typeof<XElement>, anyTypeWithAnnotationsProp.PropertyType)
    let hasEmptyDefinitionProp = rootType.GetProperty("HasEmptyDefinition")
    let hasEmptyDefinitionPropType = typeof<Elements.DefinedTypes.EuXRoadTest.HasElements.HasEmptyDefinitionType>
    Assert.IsNotNull(hasEmptyDefinitionProp)
    Assert.AreEqual(hasEmptyDefinitionPropType, hasEmptyDefinitionProp.PropertyType)
    let ctor = hasEmptyDefinitionPropType.GetConstructor([||])
    Assert.IsNotNull(ctor)
