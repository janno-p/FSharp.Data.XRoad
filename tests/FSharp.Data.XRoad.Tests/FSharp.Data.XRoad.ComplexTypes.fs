module FSharp.Data.XRoad.ComplexTypes

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open FsUnit.Xunit
open NodaTime
open Xunit
open System.Reflection

type ComplexTypes = GenerateTypesFromString<"""
<wsdl:definitions targetNamespace="http://test.x-road.eu/" xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" xmlns:tns="http://test.x-road.eu/" xmlns:xrd="http://x-road.eu/xsd/xroad.xsd" xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/">
    <wsdl:types>
        <xs:schema targetNamespace="http://test.x-road.eu/" xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:complexType name="AbstractType" abstract="true">
                <xs:sequence>
                    <xs:element name="BaseProp" type="xs:dateTime" />
                </xs:sequence>
            </xs:complexType>
            <xs:complexType name="DerivedType">
                <xs:complexContent>
                    <xs:extension base="tns:AbstractType">
                        <xs:sequence>
                            <xs:element name="DerivedOwnProp" type="xs:string" />
                        </xs:sequence>
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
            <xs:complexType name="WithNestedTypes">
                <xs:sequence>
                    <xs:element name="Value">
                        <xs:complexType>
                            <xs:sequence />
                        </xs:complexType>
                    </xs:element>
                </xs:sequence>
            </xs:complexType>
            <xs:complexType name="Person">
                <xs:choice>
                    <xs:element name="Employee" type="xs:string" />
                    <xs:element name="Member" type="xs:long" />
                    <xs:element name="Unknown">
                        <xs:complexType>
                            <xs:sequence>
                                <xs:element name="Description" type="xs:string" />
                            </xs:sequence>
                        </xs:complexType>
                    </xs:element>
                </xs:choice>
            </xs:complexType>
            <xs:complexType name="WithOptionalSimpleType">
                <xs:sequence>
                    <xs:element name="Property" type="xs:int" minOccurs="0" />
                </xs:sequence>
            </xs:complexType>
            <xs:complexType name="WithOptionalComplexType">
                <xs:sequence>
                    <xs:element name="Property" type="tns:Person" minOccurs="0" />
                </xs:sequence>
            </xs:complexType>
            <xs:complexType name="ComplexTypeWithElementName">
                <xs:sequence />
            </xs:complexType>
            <xs:complexType name="TypeWithInvalidTypeReference">
                <xs:sequence>
                    <xs:element name="Prop" type="tns:NonExistingType" />
                </xs:sequence>
            </xs:complexType>
            <xs:element name="ComplexTypeWithElementName">
                <xs:complexType>
                    <xs:sequence>
                        <xs:element name="response" type="tns:ComplexTypeWithElementName" />
                    </xs:sequence>
                </xs:complexType>
            </xs:element>
        </xs:schema>
    </wsdl:types>
    <wsdl:message name="ComplexTypeWithElementName">
        <wsdl:part name="body" element="tns:ComplexTypeWithElementName"/>
    </wsdl:message>
    <wsdl:portType name="XRoadAdapterPortType">
        <wsdl:operation name="ComplexTypeWithElementName">
            <wsdl:input message="tns:ComplexTypeWithElementName"/>
            <wsdl:output message="tns:ComplexTypeWithElementName"/>
        </wsdl:operation>
    </wsdl:portType>
    <wsdl:binding name="XRoadBinding" type="tns:XRoadAdapterPortType">
        <soap:binding style="document" transport="http://schemas.xmlsoap.org/soap/http" />
        <wsdl:operation name="ComplexTypeWithElementName">
            <xrd:version>v1</xrd:version>
            <wsdl:input>
                <soap:body use="literal" />
            </wsdl:input>
            <wsdl:output>
                <soap:body use="literal" />
            </wsdl:output>
        </wsdl:operation>
    </wsdl:binding>
    <wsdl:service name="XRoadService">
        <wsdl:port name="MainPort" binding="tns:XRoadBinding">
            <soap:address location="http://example.org/xroad-endpoint" />
        </wsdl:port>
    </wsdl:service>
</wsdl:definitions>""">

[<Fact>]
let ``generates correct abstract base type`` () =
    let typ = typeof<ComplexTypes.DefinedTypes.EuXRoadTest.AbstractType>
    typ.IsAbstract |> should be True

    let defaultCtor = typ.GetConstructor(BindingFlags.NonPublic ||| BindingFlags.Instance, null, [||], [||])
    defaultCtor |> should not' (be Null)
    defaultCtor.IsFamily |> should be True

    typ.GetCustomAttributes() |> should haveLength 1

    typ |> assertTypeAttribute "AbstractType" "http://test.x-road.eu/" false LayoutKind.Sequence

[<Fact>]
let ``generates correct simpleContent type`` () =
    let typ = typeof<ComplexTypes.DefinedTypes.EuXRoadTest.ShoeSize>
    typ.IsAbstract |> should be False

    let defaultCtor = typ.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [||], [||])
    defaultCtor |> should not' (be Null)

    let baseValueProp = typ.GetProperty("BaseValue")
    baseValueProp |> should not' (be Null)
    baseValueProp.CanRead |> should be True
    baseValueProp.CanWrite |> should be True
    baseValueProp.PropertyType |> should be (sameAs typeof<bigint>)

    let shoeSize = ComplexTypes.DefinedTypes.EuXRoadTest.ShoeSize()
    shoeSize.BaseValue <- 1234I
    shoeSize.BaseValue |> should equal 1234I

    typ.GetCustomAttributes() |> should haveLength 1

    typ |> assertTypeAttribute "ShoeSize" "http://test.x-road.eu/" false LayoutKind.Sequence

[<Fact>]
let ``generates correct derived type`` () =
    let typ = typeof<ComplexTypes.DefinedTypes.EuXRoadTest.DerivedType>
    typ.IsAbstract |> should be False

    let baseTy = typ.BaseType
    baseTy |> should be (sameAs typeof<ComplexTypes.DefinedTypes.EuXRoadTest.AbstractType>)

    let defaultCtor = typ.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [||], [||])
    defaultCtor |> should not' (be Null)

    typ.GetCustomAttributes() |> should haveLength 1
    typ |> assertTypeAttribute "DerivedType" "http://test.x-road.eu/" false LayoutKind.Sequence

    let dateTime = OffsetDateTime(LocalDateTime(2000, 1, 1, 0, 0), Offset.FromHours(2))

    let derivedTypeInstance = ComplexTypes.DefinedTypes.EuXRoadTest.DerivedType()
    derivedTypeInstance.DerivedOwnProp <- "value"
    derivedTypeInstance.BaseProp <- dateTime

    derivedTypeInstance |> should not' (be Null)
    derivedTypeInstance.DerivedOwnProp |> should equal "value"
    derivedTypeInstance.BaseProp |> should equal dateTime
    derivedTypeInstance |> should be (instanceOfType<ComplexTypes.DefinedTypes.EuXRoadTest.AbstractType>)

[<Fact>]
let ``Can generate nested anonymous types`` () =
    let withNested = ComplexTypes.DefinedTypes.EuXRoadTest.WithNestedTypes()
    let nestedValue = ComplexTypes.DefinedTypes.EuXRoadTest.WithNestedTypes.ValueType()
    withNested.Value <- nestedValue
    withNested.Value |> should be (sameAs nestedValue)

[<Fact>]
let ``Can generate choice types`` () =
    let choiceType = ComplexTypes.DefinedTypes.EuXRoadTest.Person()
    choiceType |> should not' (be Null)

    choiceType.Choice1 <- ComplexTypes.DefinedTypes.EuXRoadTest.Person.Choice1Type.NewEmployee("test")
    choiceType.Choice1.TryGetEmployee() |> should equal (Optional.Option.Some<string>("test"))
    choiceType.Choice1.TryGetMember() |> should equal (Optional.Option.None<int64>())

    let unknownValue = ComplexTypes.DefinedTypes.EuXRoadTest.Person.UnknownType(Description = "another")
    let complexChoice = ComplexTypes.DefinedTypes.EuXRoadTest.Person.Choice1Type.NewUnknown(unknownValue)

    let result = complexChoice.TryGetUnknown()
    result.HasValue |> should be True

    let u = result.ValueOr(fun _ -> null)
    u |> should not' (be Null)
    u |> should be (sameAs unknownValue)

[<Fact>]
let ``Can detect invalid types`` () =
    let invalidType = ComplexTypes.InvalidTypes.EuXRoadTest.TypeWithInvalidTypeReference.Errors
    invalidType |> should equal "Invalid type name `SchemaType({http://test.x-road.eu/}NonExistingType)`: type not found in cache."

[<Fact>]
let ``Can use optional property`` () =
    let withOptionalProperty = ComplexTypes.DefinedTypes.EuXRoadTest.WithOptionalSimpleType()
    let v = Optional.Option.Some<int>(1)
    withOptionalProperty.Property <- v
    withOptionalProperty.Property |> should equal v

[<Fact>]
let ``Can use optional property with complex value`` () =
    let withOptionalProperty = ComplexTypes.DefinedTypes.EuXRoadTest.WithOptionalComplexType()
    let v = Optional.Option.Some<ComplexTypes.DefinedTypes.EuXRoadTest.Person>(ComplexTypes.DefinedTypes.EuXRoadTest.Person())
    withOptionalProperty.Property <- v
    v.HasValue |> should be True
    withOptionalProperty.Property.HasValue |> should be True
    withOptionalProperty.Property.ValueOr(fun _ -> null) |> should be (sameAs (v.ValueOr(fun _ -> null)))
