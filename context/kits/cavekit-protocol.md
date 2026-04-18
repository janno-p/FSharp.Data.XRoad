---
created: "2026-04-16T00:00:00Z"
last_edited: "2026-04-18T15:30:00Z"
---

# Cavekit: SOAP Protocol & Runtime

## Scope

This cavekit covers runtime SOAP protocol operations: constructing SOAP envelopes, sending HTTP requests to X-Road services, receiving and parsing SOAP responses, and handling faults. It includes:
- SOAP 1.1 envelope construction with X-Road v6 headers (client, service, protocol version)
- HTTP POST request execution with security certificate handling
- SOAP Fault detection and exception raising
- Multipart SOAP response parsing (body + attachments)
- Response event signaling (ResponseReady)
- Stream-based serialized message handling

This cavekit does NOT include IL-based serialization (cavekit-serialization), HTTP transport library details (cavekit-http-transport), or schema parsing (cavekit-schema-parsing).

## Requirements

### R1: Construct SOAP Envelope
**Description:** SOAP 1.1 envelopes with X-Road v6 headers must be properly constructed.

**Acceptance Criteria:**
- [ ] Envelope element uses correct SOAP 1.1 namespace (http://schemas.xmlsoap.org/soap/envelope/)
- [ ] Envelope wraps Header and Body elements
- [ ] Header contains protocolVersion element (set to "4.0" for X-Road v6)
- [ ] Header contains unique id element (UUID generated per request)
- [ ] Header contains service element with service identifier (instance, member, subsystem, code, version)
- [ ] Header contains client element with client identifier (instance, member, subsystem)
- [ ] Namespace declarations for X-Road are present (xro, iden prefixes)
- [ ] Body element contains operation-specific request content
- [ ] Envelope uses UTF-8 encoding
- [ ] Subsystem codes are optional (omitted if empty)
- [ ] Service versions are optional (omitted if empty)

**Dependencies:** cavekit-core-types (identifier types)

### R2: Send HTTP POST Request
**Description:** SOAP envelopes must be sent as HTTP POST requests to the X-Road security server.

**Acceptance Criteria:**
- [ ] HTTP POST request method is used
- [ ] Content-Type header is set to "text/xml; charset=utf-8"
- [ ] SOAPAction header is set to empty string
- [ ] Request body is the SOAP envelope (serialized to stream)
- [ ] Request URI is the X-Road security server endpoint
- [ ] Client certificates are attached to the request if configured
- [ ] Request is sent synchronously
- [ ] HTTP response is received and read into memory stream

**Dependencies:** cavekit-serialization (envelope serialization), cavekit-core-types

### R3: Parse SOAP Response Envelope
**Description:** SOAP response envelopes must be parsed to extract the response content.

**Acceptance Criteria:**
- [ ] Response XML is parsed into XmlReader
- [ ] Envelope element is located in SOAP namespace
- [ ] Body element is located within Envelope
- [ ] Body content element is the first child of Body (operation response wrapper)
- [ ] Content is extracted for deserialization
- [ ] Missing Envelope or Body fails with clear error

**Dependencies:** R2 (receive response)

### R4: Detect and Raise SOAP Faults
**Description:** SOAP Fault responses must be detected and converted to exceptions.

**Acceptance Criteria:**
- [ ] Fault element in Body is detected (XPath expression matches)
- [ ] faultCode element is extracted
- [ ] faultstring element is extracted
- [ ] Exception is raised with fault code and fault string as message
- [ ] Exception type indicates SOAP fault (XRoadFault or similar)
- [ ] Normal (non-fault) response passes through without error

**Dependencies:** R3 (parse response)

### R5: Handle Multipart SOAP Responses
**Description:** Responses with attachments (SwA/MTOM) are parsed to separate body from attachments. The low-level multipart MIME parsing (boundary detection, part splitting, Content-Transfer-Encoding decoding including base64) is delegated to the HTTP Transport layer (cavekit-http-transport R3). This domain is responsible for consuming the parsed parts and routing body vs. attachments into the appropriate protocol slots.

**Acceptance Criteria:**
- [ ] Content-Type header is checked for multipart/related boundary marker
- [ ] Single-part responses (no attachments) are handled without error
- [ ] Multipart parsing (boundary extraction, part splitting, base64/binary decoding) is handled by the HTTP transport layer — not re-implemented here
- [ ] Parsed attachment parts are stored by Content-ID for retrieval during deserialization
- [ ] Attachments are available to deserialization logic after response parse completes
- [ ] MTOM deserializers can retrieve attachments by Content-ID reference

**Dependencies:** cavekit-http-transport R3 (multipart MIME parsing, base64 decoding), R3 (parse SOAP response envelope)

### R6: Signal Response Ready Event
**Description:** After response is received but before deserialization, a ResponseReady event is signaled.

**Acceptance Criteria:**
- [ ] ResponseReady event is raised on the AbstractEndpointDeclaration
- [ ] Event includes the response object and request context
- [ ] Event is raised before deserialization starts
- [ ] Event allows subscribers to inspect raw response if needed
- [ ] Event is raised even if response is a SOAP Fault

**Dependencies:** cavekit-core-types (AbstractEndpointDeclaration)

### R7: Deserialize Response to CLR Objects
**Description:** Parsed response XML is deserialized to CLR objects using generated delegates.

**Acceptance Criteria:**
- [ ] Response body element is passed to the operation's deserializer delegate
- [ ] Deserializer constructs response object with deserialized properties
- [ ] Attachments are available to deserializer if MTOM/SwA is used
- [ ] Deserialization errors are reported with clear message
- [ ] Response is returned to the caller

**Dependencies:** cavekit-serialization (R2 deserializers), R5 (attachments)

### R8: Handle Client Certificates
**Description:** X-Road services may require client certificate authentication.

**Acceptance Criteria:**
- [ ] Client certificates are configured on AbstractEndpointDeclaration
- [ ] Client certificates are attached to HTTP request
- [ ] Certificate selection is based on thumbprint or other criteria
- [ ] Missing required certificate results in HTTP 401/403 error
- [ ] Certificate validation errors are propagated clearly

**Dependencies:** cavekit-core-types (endpoint configuration)

### R9: Server Certificate Validation [GAP]
**Description:** HTTPS requests to X-Road must validate server certificates.

**Acceptance Criteria:**
- [ ] Server certificate is validated against system trust store by default
- [ ] Self-signed certificates are rejected by default
- [ ] Expired certificates are rejected
- [ ] Hostname mismatches are rejected
- [ ] Optionally, a specific trusted certificate can be pinned
- [ ] Certificate validation errors are reported clearly

**Dependencies:** None (network layer)

**Status:** [GAP] Current code: Server certificate validation disabled in Protocol.fs line 88. `ServerCertificateValidationCallback` can accept invalid certs if configured.

### R10: HTTP Timeout
**Description:** HTTP requests must timeout after a reasonable duration.

**Acceptance Criteria:**
- [ ] HTTP request timeout is configurable
- [ ] Default timeout is reasonable (e.g., 30 seconds)
- [ ] Timeout is enforced on `request.GetResponse()` call
- [ ] Timeout raises an exception with clear message

**Dependencies:** None (network layer)

### R11: Stream Disposal and Resource Cleanup
**Description:** HTTP response streams and other resources must be properly disposed.

**Acceptance Criteria:**
- [ ] WebResponse is disposed after response processing
- [ ] Response body streams are disposed
- [ ] Serialization context streams are disposed
- [ ] No resource leaks on normal or exception paths
- [ ] Using statements or try/finally ensure cleanup

**Dependencies:** None (resource management)

### R12: Response Body Streaming [GAP]
**Description:** Large responses should be streamed (not loaded entirely into memory) where possible.

**Acceptance Criteria:**
- [ ] Response body stream is not forced into memory immediately [KNOWN LIMITATION: response is currently copied to MemoryStream before fault detection and multipart parsing — both require seekability; full streaming would require major redesign]
- [ ] Deserialization can read from the stream incrementally
- [ ] XmlReader is used for streaming XML parsing (not XDocument.Load)
- [ ] Large attachments are not buffered entirely in memory

**Dependencies:** None (performance)

**Status:** [KNOWN LIMITATION] Response is buffered to MemoryStream (Protocol.fs line 55) to enable seekability for fault checking and multipart parsing. XmlReader-based incremental deserialization is implemented; full streaming of the initial response copy is deferred.

### R13: Request/Response Logging (Optional)
**Description:** For debugging, requests and responses may be logged via event subscription.

**Acceptance Criteria:**
- [ ] Request envelope can be logged via IXRoadRequest.Save(stream)
- [ ] Response envelope can be logged via IXRoadResponse.Save(stream)
- [ ] Logging is subscriber-based: RequestReady/ResponseReady events allow interception without built-in file/debug infrastructure
- [ ] Logging is zero-overhead when no subscribers are registered
- [ ] Performance impact of logging is minimal when disabled

**Dependencies:** R14 (RequestReady event), R6 (ResponseReady event)

### R14: Request Ready Event Signaling
**Description:** Before a request is sent to the server, a RequestReady event is signaled for pre-send inspection and logging.

**Acceptance Criteria:**
- [ ] RequestReady event is raised on AbstractEndpointDeclaration before HTTP send
- [ ] Event includes the request object (IXRoadRequest) for inspection
- [ ] Event is raised after the SOAP envelope is fully constructed
- [ ] Event is symmetrical with ResponseReady (R6) — both enable subscriber-based observability
- [ ] Subscribers can call IXRoadRequest.Save(stream) to capture the outgoing envelope

**Dependencies:** cavekit-core-types (AbstractEndpointDeclaration)

## Out of Scope

- SOAP 1.2 (only 1.1 supported)
- WSDL parsing and service discovery (see cavekit-type-provider)
- HTTP transport library implementation (using WebRequest/HttpWebRequest from .NET BCL)
- TLS certificate chain validation (delegated to system trust store)
- Proxy configuration and SOCKS support
- Compression (gzip, deflate)
- DNS resolution
- Connection pooling and keep-alive management
- HTTP/2 or HTTP/3
- WS-* standards (WS-Security, WS-Addressing, etc.)

## Cross-References

- **cavekit-serialization**: Serializes request objects to SOAP envelope; deserializes response
- **cavekit-http-transport**: Low-level HTTP operations (also multipart parsing, overlaps with R5)
- **cavekit-core-types**: AbstractEndpointDeclaration, XRoadHeader, identifier types
- **cavekit-type-generation**: Generated service types produce IXRoadRequest/IXRoadResponse

## Source Traceability

**Source files:**
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Protocol.fs` (293 lines) — SOAP envelope construction, request/response handling

**Related files:**
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.fs` — Core protocol types (XRoadHeader, XRoadRequest, XRoadResponse)
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Emit.fs` — Serialization delegates for request/response
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Http.fs` — HTTP transport (shared multipart parsing)

**Typical flow:**
1. User calls generated service method (e.g., `service.GetPerson(input)`)
2. Service method implementation constructs XRoadRequest
3. SOAP envelope is built with request content serialized (R1)
4. HTTP POST is sent with SOAP envelope (R2)
5. Response is received (R3)
6. SOAP Fault is checked (R4), exception raised if present
7. Multipart response is parsed if needed (R5)
8. ResponseReady event is signaled (R6)
9. Response body is deserialized to CLR object (R7)
10. Result is returned to user

## Changes
- 2026-04-18: R10, R11 — Removed [GAP] status; both resolved in protocol build loop (T-006, T-007)
- 2026-04-18: R12 — Updated [GAP] to [KNOWN LIMITATION]; clarified that MemoryStream buffering is required for seekability
- 2026-04-18: R13 — Updated acceptance criteria to reflect event-based subscriber model (not file-based config)
- 2026-04-18: Added R14 (RequestReady Event Signaling) — formalizes over-built feature from protocol loop (finding F-over-built-001)
- 2026-04-18: R5 — Added criterion: multipart request serialization (serializeMultipartMessage) must be covered by test (F-PF-013); R11 — noted StreamWriter ownership requirement (F-PF-012)
