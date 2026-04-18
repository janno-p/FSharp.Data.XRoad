---
created: "2026-04-18T00:00:00Z"
last_edited: "2026-04-18T15:00:00Z"
---
# Implementation Tracking: protocol

Build site: context/plans/build-site-protocol.md

| Task | Status | Notes |
|------|--------|-------|
| T-001 | DONE | SOAP 1.1 envelope construction verified: namespace, Header/Body, protocolVersion, UUID id, client/service elements, xro/id namespace declarations, UTF-8, optional subsystem/version — 11 tests |
| T-002 | DONE | Client cert config verified: AuthenticationCertificates, AcceptedServerCertificate on AbstractEndpointDeclaration, cert attachment to HttpWebRequest — 3 tests |
| T-003 | DONE | HTTP POST behavior verified: Method=POST, Content-Type, SOAPAction="", URI, body contains SOAP XML — 4 tests |
| T-004 | DONE | SOAP response parsing: extracted parseSoapEnvelopeBody internal helper; 5 tests covering R3.a-f (Envelope/Body location, first-child content, missing-element errors) |
| T-005 | DONE | Server cert validation behavior verified: no custom callback without AcceptedServerCertificate (default system validation), AcceptedServerCertificate settable for pinning — 2 tests |
| T-006 | DONE | Added Timeout property (default 30000ms) to AbstractEndpointDeclaration; wired request.Timeout in XRoadRequest constructor — 3 tests |
| T-007 | DONE | Resource cleanup verified: XRoadRequest implements IDisposable, use binding works — 2 tests |
| T-008 | DONE | R12 streaming: 3 tests — XmlReader return type, incremental reads, large-doc no-buffer |
| T-009 | DONE | R4 fault detection: 5 tests — checkFaultInStream normal pass-through, soap:Fault XPath, faultCode/faultString extraction, XRoadFault type, Message=faultString |
| T-010 | DONE | R5 multipart: 4 tests — single-part pass-through, boundary split + attachment by Content-ID, SerializerContext cid: lookup, MTOM IsMtomMessage flag |
| T-011 | DONE | R6 ResponseReady: 2 tests — event fires on TriggerResponseReady, args carry RequestId/ServiceCode/ServiceVersion/Header |
| T-012 | DONE | R7 deserialization: 4 tests via HttpListener — deserializer at operation element, result returned, R6.c event before deserializer, R7.d exception propagation |
| T-013 | DONE | R13 logging (optional): 3 tests — IXRoadRequest.Save, IXRoadResponse.Save, RequestReady event-based logging |
| T-014 | DONE | XmlReaderSettings(CloseInput=false) in parseSoapEnvelopeBody — fixes double-dispose ownership bug (PF-001) |
| T-015 | DONE | nodeToString uses .Value not .InnerXml; fault fallback raises XRoadFault not generic Exception (PF-002, PF-003) |
| T-016 | DONE | HttpListener port binding uses retry loop (max 5 attempts); server thread captures exceptions in mutable ref (PF-004, PF-005) |
| T-017 | DONE | R6.e test added — verifies ResponseReady fires before XRoadFault raised on fault path (PF-006); total protocol tests: 218 |
| T-018 | DONE | checkFaultInStream uses CloseInput=false (F-PF-009); startSoapServer exposes serverError ref, all callers check it (F-PF-010); TcpListener instances use `new` keyword for FS0760 (F-PF-011); Build P, Tests P(218) |
| T-019 | DONE | serializeMultipartMessage uses `use writer = ... leaveOpen=true` (F-PF-012); R5.g multipart request test added (F-PF-013); faultServerError checked in R6.e (F-PF-014); Build P, Tests P(219) |
