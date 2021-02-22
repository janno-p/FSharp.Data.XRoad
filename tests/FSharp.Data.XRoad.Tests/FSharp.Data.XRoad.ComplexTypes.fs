module FSharp.Data.XRoad.ComplexTypes

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Attributes
open NodaTime
open NUnit.Framework
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

[<Test>]
let ``generates correct abstract base type`` () =
    let typ = typeof<ComplexTypes.DefinedTypes.Eu_XRoad_Test.AbstractType>
    Assert.IsTrue(typ.IsAbstract, "Abstract type should be define as abstract.")

    let defaultCtor = typ.GetConstructor(BindingFlags.NonPublic ||| BindingFlags.Instance, null, [||], [||])
    Assert.IsNotNull(defaultCtor, "Abstract type should have protected default constructor.")
    Assert.IsTrue(defaultCtor.IsFamily, "Abstract type should have protected default constructor.")

    Assert.AreEqual(1, typ.GetCustomAttributes() |> Seq.length)
    typ |> assertTypeAttribute "AbstractType" "http://test.x-road.eu/" false LayoutKind.Sequence

[<Test>]
let ``generates correct simpleContent type`` () =
    let typ = typeof<ComplexTypes.DefinedTypes.Eu_XRoad_Test.ShoeSize>
    Assert.IsFalse(typ.IsAbstract, "ShoeSize type should not be define as abstract.")

    let defaultCtor = typ.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [||], [||])
    Assert.IsNotNull(defaultCtor, "ShoeSize type should have public default constructor.")

    let baseValueProp = typ.GetProperty("BaseValue")
    Assert.IsNotNull(baseValueProp, "ShoeSize type should have BaseValue property")
    Assert.IsTrue(baseValueProp.CanRead, "BaseValue property should be readable")
    Assert.IsTrue(baseValueProp.CanWrite, "BaseValue property should be writeable")
    Assert.AreEqual(typeof<bigint>, baseValueProp.PropertyType)

    let shoeSize = ComplexTypes.DefinedTypes.Eu_XRoad_Test.ShoeSize()
    shoeSize.BaseValue <- 1234I
    Assert.AreEqual(1234I, shoeSize.BaseValue)

    Assert.AreEqual(1, typ.GetCustomAttributes() |> Seq.length)
    typ |> assertTypeAttribute "ShoeSize" "http://test.x-road.eu/" false LayoutKind.Sequence

[<Test>]
let ``generates correct derived type`` () =
    let typ = typeof<ComplexTypes.DefinedTypes.Eu_XRoad_Test.DerivedType>
    Assert.IsFalse(typ.IsAbstract, "DerivedType type should not be define as abstract.")

    let baseTy = typ.BaseType
    Assert.AreEqual(baseTy, typeof<ComplexTypes.DefinedTypes.Eu_XRoad_Test.AbstractType>, "DerivedType should be based on AbstractType")

    let defaultCtor = typ.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [||], [||])
    Assert.IsNotNull(defaultCtor, "DerivedType type should have public default constructor.")

    Assert.AreEqual(1, typ.GetCustomAttributes() |> Seq.length)
    typ |> assertTypeAttribute "DerivedType" "http://test.x-road.eu/" false LayoutKind.Sequence

    let dateTime = OffsetDateTime(LocalDateTime(2000, 1, 1, 0, 0), Offset.FromHours(2))

    let derivedTypeInstance = ComplexTypes.DefinedTypes.Eu_XRoad_Test.DerivedType()
    derivedTypeInstance.DerivedOwnProp <- "value"
    derivedTypeInstance.BaseProp <- dateTime

    Assert.IsNotNull(derivedTypeInstance)
    Assert.AreEqual("value", derivedTypeInstance.DerivedOwnProp)
    Assert.AreEqual(dateTime, derivedTypeInstance.BaseProp)
    Assert.IsInstanceOf<ComplexTypes.DefinedTypes.Eu_XRoad_Test.AbstractType>(derivedTypeInstance)

[<Test>]
let ``Can generate nested anonymous types`` () =
    let withNested = ComplexTypes.DefinedTypes.Eu_XRoad_Test.WithNestedTypes()
    let nestedValue = ComplexTypes.DefinedTypes.Eu_XRoad_Test.WithNestedTypes.ValueType()
    withNested.Value <- nestedValue
    Assert.AreSame(nestedValue, withNested.Value)

[<Test>]
let ``Can generate choice types`` () =
    let choiceType = ComplexTypes.DefinedTypes.Eu_XRoad_Test.Person()
    Assert.IsNotNull(choiceType)

    choiceType.Choice1 <- ComplexTypes.DefinedTypes.Eu_XRoad_Test.Person.Choice1Type.NewEmployee("test")
    Assert.AreEqual(Optional.Option.Some<string>("test"), choiceType.Choice1.TryGetEmployee())
    Assert.AreEqual(Optional.Option.None<int64>(), choiceType.Choice1.TryGetMember())

    let unknownValue = ComplexTypes.DefinedTypes.Eu_XRoad_Test.Person.UnknownType(Description = "another")
    let complexChoice = ComplexTypes.DefinedTypes.Eu_XRoad_Test.Person.Choice1Type.NewUnknown(unknownValue)

    let result = complexChoice.TryGetUnknown()
    Assert.IsTrue(result.HasValue)

    let u = result.ValueOr(fun _ -> null)
    Assert.IsNotNull(u)
    Assert.AreSame(u, unknownValue)

[<Test>]
let ``Can detect invalid types`` () =
    let invalidType = ComplexTypes.InvalidTypes.Eu_XRoad_Test.TypeWithInvalidTypeReference.Errors
    Assert.AreEqual("Invalid type name `SchemaType({http://test.x-road.eu/}NonExistingType)`: type not found in cache.", invalidType)

[<Test>]
let ``Can use optional property`` () =
    let withOptionalProperty = ComplexTypes.DefinedTypes.Eu_XRoad_Test.WithOptionalSimpleType()
    let v = Optional.Option.Some<int>(1)
    withOptionalProperty.Property <- v
    Assert.AreEqual(v, withOptionalProperty.Property)

[<Test>]
let ``Can use optional property with complex value`` () =
    let withOptionalProperty = ComplexTypes.DefinedTypes.Eu_XRoad_Test.WithOptionalComplexType()
    let v = Optional.Option.Some<ComplexTypes.DefinedTypes.Eu_XRoad_Test.Person>(ComplexTypes.DefinedTypes.Eu_XRoad_Test.Person())
    withOptionalProperty.Property <- v
    Assert.IsTrue(v.HasValue)
    Assert.IsTrue(withOptionalProperty.Property.HasValue)
    Assert.AreSame(v.ValueOr((fun _ -> null)), withOptionalProperty.Property.ValueOr((fun _ -> null)))
