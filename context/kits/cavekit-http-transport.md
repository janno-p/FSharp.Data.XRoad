---
created: "2026-04-16T00:00:00Z"
last_edited: "2026-04-16T00:00:00Z"
---

# Cavekit: HTTP Transport & Security

## Scope

This cavekit covers all HTTP network operations performed by the design-time assembly when communicating with X-Road security servers. It includes:
- Document download (WSDL, XSD, producer lists, service lists)
- Caching and file storage of downloaded documents
- HTTPS certificate validation
- TLS/SSL protocol configuration
- MIME multipart response parsing
- SOAP/XML request/response formatting for meta-service calls (listMethods, listClients, listCentralServices)

This cavekit does NOT include runtime SOAP message sending — see cavekit-protocol for that.

## Requirements

### R1: Fetch WSDL and XSD Documents Over HTTP/HTTPS
**Description:** The system must download schema documents (WSDL, XSD) referenced by a WSDL URL or X-Road security server URI.

**Acceptance Criteria:**
- [ ] HTTP GET requests to XML document URLs return document content
- [ ] HTTPS GET requests to XML document URLs return document content
- [ ] Fetching fails with a clear error if the URI is malformed
- [ ] Fetching fails with a clear error if the remote server returns a non-2xx HTTP status
- [ ] Downloaded XML content is parsed into XDocument in-memory structure
- [ ] File paths are resolved as URIs if they reference local files

**Dependencies:** None (foundational)

### R2: Cache Downloaded Documents on Disk
**Description:** Recently downloaded documents must be cached in the temporary file system to avoid re-downloading identical resources.

**Acceptance Criteria:**
- [ ] First download of a URI writes content to a temp file
- [ ] Second request for the same URI loads content from cached file (no network call)
- [ ] Cache key is derived from the complete URI
- [ ] Cache can be explicitly cleared by passing a `refresh=true` flag to bypass cache
- [ ] Cached files are stored in the system temp directory

**Dependencies:** R1 (must fetch before caching)

### R3: Support Multipart MIME Responses
**Description:** SOAP responses from X-Road may be multipart/related, containing a primary XML body and binary attachments (SwA/MTOM format).

**Acceptance Criteria:**
- [ ] Content-Type header is parsed to detect multipart/related boundary marker
- [ ] Single-part responses (non-multipart) are handled without error
- [ ] Multipart boundary is extracted from Content-Type header
- [ ] Body part and attachment parts are separated by boundary marker
- [ ] Content-Transfer-Encoding header is parsed (base64, quoted-printable, 7bit, 8bit, binary)
- [ ] base64-encoded attachment content is decoded to bytes
- [ ] Other encodings are passed through without decoding
- [ ] Each attachment includes a Content-ID header for identification
- [ ] Multipart parsing fails with a clear error if boundary marker is not found
- [ ] Multipart parsing fails with a clear error if stream ends unexpectedly
- [ ] Multipart parsing fails with a clear error if an unknown Content-Transfer-Encoding is encountered

**Dependencies:** None (orthogonal to document fetching)

### R4: Validate Server Certificates [GAP]
**Description:** HTTPS requests to X-Road security servers must validate the server's TLS certificate to prevent man-in-the-middle attacks.

**Acceptance Criteria:**
- [ ] HTTPS requests with valid, signed certificates are accepted
- [ ] HTTPS requests with self-signed certificates are rejected by default
- [ ] HTTPS requests with expired certificates are rejected
- [ ] HTTPS requests with hostname mismatches are rejected
- [ ] Certificate validation can be configured to accept a specific certificate (for testing/development)
- [ ] Certificate validation respects certificate trust chains (intermediate CA certs)

**Dependencies:** None (separate concern)

**Status:** [GAP] Current code: `fun _ _ _ _ -> true` disables validation globally. Certificate hostname validation not implemented.

### R5: Download X-Road Producer List
**Description:** Download and parse the `listClients` meta-service response from a security server to enumerate all registered members, member classes, and subsystems.

**Acceptance Criteria:**
- [ ] GET request to `{securityServerUri}listClients?xRoadInstance={instance}` succeeds
- [ ] SOAP Fault responses are detected and raised as exceptions with fault code and fault string
- [ ] XML response is parsed to extract member class names
- [ ] For each member class, member codes and display names are extracted
- [ ] For each member, associated subsystem codes are extracted and grouped
- [ ] Result is a list of member classes, each containing members, each containing subsystems
- [ ] Members are sorted by name (display name)
- [ ] Subsystems within a member are sorted by subsystem code

**Dependencies:** R1 (HTTP fetch), R3 (multipart parsing)

### R6: Download X-Road Central Service List
**Description:** Download and parse the `listCentralServices` meta-service response from a security server to enumerate all central services.

**Acceptance Criteria:**
- [ ] GET request to `{securityServerUri}listCentralServices?xRoadInstance={instance}` succeeds
- [ ] XML response is parsed to extract service codes
- [ ] Service codes are returned as a sorted list
- [ ] Empty result (no central services) is handled without error

**Dependencies:** R1 (HTTP fetch)

### R7: Download Service Method List
**Description:** Send a SOAP request to a service's `listMethods` operation to enumerate all methods exposed by that service.

**Acceptance Criteria:**
- [ ] SOAP request is constructed with client and service identifiers in the header
- [ ] SOAP request body contains a `listMethods` operation
- [ ] POST request with SOAP body is sent to the security server URI
- [ ] Response envelope is parsed to extract service definitions
- [ ] SOAP Fault responses are detected and raised as exceptions with fault code and fault string
- [ ] Each method is returned as an XRoadServiceIdentifier with service code and optional version
- [ ] XRoad v6 header namespace and structure are correctly formatted

**Dependencies:** R1 (HTTP POST), R3 (multipart response)

### R8: TLS Protocol Configuration [GAP]
**Description:** HTTPS connections must use secure TLS versions (1.2+) and exclude deprecated protocols.

**Acceptance Criteria:**
- [ ] Default configuration enables TLS 1.2
- [ ] Default configuration enables TLS 1.3 (if available)
- [ ] Default configuration disables TLS 1.0
- [ ] Default configuration disables TLS 1.1
- [ ] Configuration can be customized per request if needed

**Dependencies:** None (security hardening)

**Status:** [GAP] Current code: `SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls` enables TLS 1.0 and 1.1.

### R9: XXE (XML External Entity) Protection [GAP]
**Description:** XML parsing must reject external entity references and DTD declarations to prevent XXE attacks.

**Acceptance Criteria:**
- [ ] XDocument.Load() is called with hardened XmlReaderSettings
- [ ] External entity declarations are forbidden
- [ ] DTD processing is disabled
- [ ] DOCTYPE declarations are rejected
- [ ] Parsing of XXE payloads fails with a clear error
- [ ] Network calls are not made during XML parsing (no entity resolution)

**Dependencies:** None (security hardening)

**Status:** [GAP] Current code: `XDocument.Load(file.OpenRead())` and `XDocument.Load(responseStream)` called without XmlReaderSettings.

### R10: Resource Cleanup & Stream Disposal [GAP]
**Description:** Streams and file handles must be properly disposed to avoid resource leaks and file lock issues.

**Acceptance Criteria:**
- [ ] File streams from cache reads are disposed after XDocument is loaded
- [ ] WebResponse objects are disposed after response processing
- [ ] StreamWriter objects are flushed and closed
- [ ] No resource leaks when exceptions occur during parsing

**Dependencies:** None (resource management)

**Status:** [GAP] Current code: `file.OpenRead()` called without using statement at line 58. WebResponse disposal missing in Protocol domain.

### R11: HTTP Request Timeout [GAP]
**Description:** HTTP requests to X-Road security servers must timeout after a reasonable duration to avoid hanging indefinitely.

**Acceptance Criteria:**
- [ ] HTTP GET requests timeout after a configured duration (e.g., 30s default)
- [ ] HTTP POST requests timeout after a configured duration
- [ ] Timeout duration is configurable
- [ ] Timeout is raised as an exception with a clear message

**Dependencies:** None (robustness)

**Status:** [GAP] Current code: `request.GetResponse()` has no timeout set. Can hang indefinitely.

### R12: SSRF (Server-Side Request Forgery) Prevention [GAP]
**Description:** When resolving schema imports, the system must restrict network access to prevent SSRF attacks.

**Acceptance Criteria:**
- [ ] Only HTTPS URIs are allowed for remote schema imports (not HTTP)
- [ ] Only registered X-Road security server origins are allowed (not arbitrary domains)
- [ ] Relative imports (local or same-origin) are allowed
- [ ] Attempts to import from disallowed origins fail with a clear error

**Dependencies:** R1 (URI resolution)

**Status:** [GAP] Current code: `getXDocument` accepts both HTTP and HTTPS. No origin validation for imported schemas.

### R13: Multipart Content Size Limits [GAP]
**Description:** Multipart attachment parsing must enforce size limits to prevent memory exhaustion or denial-of-service attacks.

**Acceptance Criteria:**
- [ ] Individual attachment size has a configurable maximum (e.g., 100MB default)
- [ ] Total multipart size has a configurable maximum
- [ ] Parsing fails with a clear error if size limit is exceeded
- [ ] Size limits are enforced during streaming (not after reading entire content)

**Dependencies:** R3 (multipart parsing)

**Status:** [GAP] Current code: Multipart parsing reads unbounded chunks into byte arrays. No size enforcement.

## Out of Scope

- Runtime SOAP message serialization and sending (see cavekit-protocol)
- Schema validation and compilation (see cavekit-schema-parsing)
- Type generation from schemas (see cavekit-type-generation)
- Local file I/O beyond cache management
- Authentication (client certificates are configured in runtime protocol code)
- Compression (gzip, deflate)
- Proxy configuration
- DNS resolution
- HTTP/2 or HTTP/3 protocol negotiation

## Cross-References

- **cavekit-schema-parsing**: Schemas fetched by HTTP transport are parsed here
- **cavekit-protocol**: Runtime SOAP sending also uses HTTP but at runtime; uses different certificate validation path
- **cavekit-test-infrastructure**: Tests may mock HTTP responses

## Source Traceability

**Source files:**
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Http.fs` (358 lines)

**Related files:**
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Wsdl.fs` — consumes downloaded documents
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Schema.fs` — consumes downloaded schemas
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Protocol.fs` — runtime SOAP transport (similar but separate)
