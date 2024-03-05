module FSharp.Data.XRoad.Elements

open FSharp.Data.XRoad
open FsUnit.Xunit
open FsUnitTyped
open System.Xml.Linq
open Xunit

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

[<Fact>]
let ``has correct type definitions for elements`` () =
    let rootType = typeof<Elements.DefinedTypes.EuXRoadTest.HasElements>
    let hasAnyTypeProp = rootType.GetProperty("HasAnyType")
    hasAnyTypeProp |> should not' (be Null)
    hasAnyTypeProp.PropertyType |> shouldEqual typeof<XElement>
    let anyTypeWithAnnotationsProp = rootType.GetProperty("AnyTypeWithAnnotations")
    anyTypeWithAnnotationsProp |> should not' (be Null)
    anyTypeWithAnnotationsProp.PropertyType |> shouldEqual typeof<XElement>
    let hasEmptyDefinitionProp = rootType.GetProperty("HasEmptyDefinition")
    let hasEmptyDefinitionPropType = typeof<Elements.DefinedTypes.EuXRoadTest.HasElements.HasEmptyDefinitionType>
    hasEmptyDefinitionProp |> should not' (be Null)
    hasEmptyDefinitionProp.PropertyType |> shouldEqual hasEmptyDefinitionPropType
    let ctor = hasEmptyDefinitionPropType.GetConstructor([||])
    ctor |> should not' (be Null)
