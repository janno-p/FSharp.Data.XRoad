module FSharp.Data.XRoad.Services

open FSharp.Data.XRoad
open FsUnit.Xunit
open FsUnitTyped
open System
open Xunit

type ServiceTypes = GenerateTypesFromString<"""
<wsdl:definitions targetNamespace="http://test.x-road.eu/" xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" xmlns:id="http://x-road.eu/xsd/identifiers" xmlns:xrd="http://x-road.eu/xsd/xroad.xsd" xmlns:tns="http://test.x-road.eu/">
    <wsdl:types>
        <xs:schema elementFormDefault="unqualified" targetNamespace="http://test.x-road.eu/" xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="helloService">
                <xs:complexType>
                    <xs:sequence>
                        <xs:element name="request">
                            <xs:complexType>
                                <xs:sequence>
                                    <xs:element name="name" type="xs:string">
                                        <xs:annotation>
                                            <xs:documentation>
                                                Name
                                            </xs:documentation>
                                        </xs:annotation>
                                    </xs:element>
                                </xs:sequence>
                            </xs:complexType>
                        </xs:element>
                    </xs:sequence>
                </xs:complexType>
            </xs:element>
            <xs:element name="helloServiceResponse">
                <xs:complexType>
                    <xs:sequence>
                        <xs:element name="request">
                            <xs:complexType>
                                <xs:sequence>
                                    <xs:element name="name" nillable="true" type="xs:string"/>
                                </xs:sequence>
                            </xs:complexType>
                        </xs:element>
                        <xs:element name="response">
                            <xs:complexType>
                                <xs:sequence>
                                    <xs:element name="message" type="xs:string">
                                        <xs:annotation>
                                            <xs:documentation>
                                                Service response
                                            </xs:documentation>
                                        </xs:annotation>
                                    </xs:element>
                                </xs:sequence>
                            </xs:complexType>
                        </xs:element>
                    </xs:sequence>
                </xs:complexType>
            </xs:element>
            <xs:element name="helloService2" type="xs:string" />
            <xs:element name="helloServiceResponse2" type="xs:string" />
        </xs:schema>
    </wsdl:types>
    <wsdl:message name="requestheader">
        <wsdl:part name="client" element="xrd:client" />
        <wsdl:part name="service" element="xrd:service" />
        <wsdl:part name="userId" element="xrd:userId" />
        <wsdl:part name="id" element="xrd:id" />
        <wsdl:part name="issue" element="xrd:issue"/>
        <wsdl:part name="protocolVersion" element="xrd:protocolVersion" />
    </wsdl:message>
    <wsdl:message name="helloService">
        <wsdl:part name="body" element="tns:helloService"/>
    </wsdl:message>
    <wsdl:message name="helloServiceResponse">
        <wsdl:part name="body" element="tns:helloServiceResponse"/>
    </wsdl:message>
    <wsdl:message name="helloService2">
        <wsdl:part name="body" element="tns:helloService2"/>
    </wsdl:message>
    <wsdl:message name="helloServiceResponse2">
        <wsdl:part name="body" element="tns:helloServiceResponse2"/>
    </wsdl:message>
    <wsdl:portType name="testServicePortType">
        <wsdl:operation name="helloService">
            <wsdl:input message="tns:helloService"/>
            <wsdl:output message="tns:helloServiceResponse"/>
        </wsdl:operation>
        <wsdl:operation name="helloService2">
            <wsdl:input message="tns:helloService2"/>
            <wsdl:output message="tns:helloServiceResponse2"/>
        </wsdl:operation>
    </wsdl:portType>
    <wsdl:binding name="testServiceBinding" type="tns:testServicePortType">
        <soap:binding style="document" transport="http://schemas.xmlsoap.org/soap/http" />
        <wsdl:operation name="helloService">
            <wsdl:documentation>
                <xrd:title xml:lang="et">Isikute nimekirja küsimine nime järgi</xrd:title>
            </wsdl:documentation>
            <soap:operation soapAction="" style="document" />
            <id:version>v1</id:version>
            <wsdl:input>
                <soap:body parts="body" use="literal"/>
                <soap:header message="tns:requestheader" part="client" use="literal"/>
                <soap:header message="tns:requestheader" part="service" use="literal"/>
                <soap:header message="tns:requestheader" part="userId" use="literal"/>
                <soap:header message="tns:requestheader" part="id" use="literal"/>
                <soap:header message="tns:requestheader" part="issue" use="literal"/>
                <soap:header message="tns:requestheader" part="protocolVersion" use="literal"/>
            </wsdl:input>
            <wsdl:output>
                <soap:body parts="body" use="literal"/>
                <soap:header message="tns:requestheader" part="client" use="literal"/>
                <soap:header message="tns:requestheader" part="service" use="literal"/>
                <soap:header message="tns:requestheader" part="userId" use="literal"/>
                <soap:header message="tns:requestheader" part="id" use="literal"/>
                <soap:header message="tns:requestheader" part="issue" use="literal"/>
                <soap:header message="tns:requestheader" part="protocolVersion" use="literal"/>
            </wsdl:output>
        </wsdl:operation>
    </wsdl:binding>
    <wsdl:binding name="withoutDefaultBinding" type="tns:testServicePortType">
        <soap:binding style="document" transport="http://schemas.xmlsoap.org/soap/http" />
        <wsdl:operation name="helloService2">
            <soap:operation soapAction="" style="document" />
            <id:version>v1</id:version>
            <wsdl:input>
                <soap:body parts="body" use="literal"/>
            </wsdl:input>
            <wsdl:output>
                <soap:body parts="body" use="literal"/>
            </wsdl:output>
        </wsdl:operation>
    </wsdl:binding>
    <wsdl:service name="testService">
        <wsdl:port binding="tns:testServiceBinding" name="testServicePort">
            <soap:address location="http://localhost:8080/Test/Endpoint" />
        </wsdl:port>
    </wsdl:service>
    <wsdl:service name="withoutDefault">
        <wsdl:port binding="tns:withoutDefaultBinding" name="withoutDefaultPort" />
    </wsdl:service>
</wsdl:definitions>""", LanguageCode="et">

[<Fact>]
let ``No default constructor for port type when soap address is not set`` () =
    let portTy = typeof<ServiceTypes.withoutDefault.withoutDefaultPort>
    portTy.GetConstructor([||]) |> should be Null
    portTy.GetConstructor([| typeof<string> |]) |> should not' (be Null)
    portTy.GetConstructor([| typeof<Uri> |]) |> should not' (be Null)

[<Fact>]
let ``Generates default constructor for port type`` () =
    let service = ServiceTypes.testService.testServicePort()
    service.Uri |> shouldEqual (Uri("http://localhost:8080/Test/Endpoint"))

[<Fact>]
let ``Generates uri string constructor for port type`` () =
    let service = ServiceTypes.testService.testServicePort("urn:test")
    service.Uri |> shouldEqual (Uri("urn:test"))

[<Fact>]
let ``Generates uri uri constructor for port type`` () =
    let uri = Uri("urn:test")
    let service = ServiceTypes.testService.testServicePort(uri)
    service.Uri |> shouldEqual uri

[<Fact>]
let ``Generates service method`` () =
    let service = ServiceTypes.testService.testServicePort()
    let req = ServiceTypes.DefinedTypes.EuXRoadTest.helloService_requestType(name="Mauno")
    service |> should not' (be Null)
    req |> should not' (be Null)
