---
created: "2026-04-17T00:00:00Z"
last_edited: "2026-04-17T00:00:00Z"
---

# Cavekit: Design-Time HTTP Transport

## Scope

This cavekit covers all HTTP operations performed by the design-time (type provider) assembly when communicating with X-Road security servers during type checking and type generation. It includes:
- WSDL and XSD document fetching via HttpClient
- Caching of downloaded documents to avoid repeated network calls
- SOAP meta-service calls (listMethods, listClients, listCentralServices) for service discovery
- Multipart MIME response parsing
- Proper stream and resource disposal
- Type provider entry point synchronization (blocking on async HTTP)
- SSRF protection for schema imports
- HTTP error handling and exception reporting to type provider diagnostics

This cavekit does NOT include runtime SOAP message sending (see cavekit-async-runtime) or the factory abstraction (see cavekit-http-client-infrastructure). It also does NOT include schema validation or type generation — those are separate domains.

## Requirements

### R1: Fetch WSDL and XSD Documents via HttpClient
**Description:** The design-time assembly must download schema documents (WSDL, XSD) from URLs referenced in type provider static parameters or schema imports, using the HttpClient obtained from IXRoadHttpClientFactory.

**Acceptance Criteria:**
- [ ] HTTP GET requests to WSDL/XSD URLs succeed and return document content
- [ ] HTTPS GET requests work with the configured HttpClient and its certificate validation
- [ ] Fetching fails with a clear, actionable error message if the URI is malformed
- [ ] Fetching fails with a clear error if the remote server returns a non-2xx HTTP status (4xx, 5xx)
- [ ] HTTP error responses include status code and reason phrase in the error message
- [ ] Downloaded XML content is parsed into XDocument in-memory structure
- [ ] File paths are resolved as `file://` URIs if they reference local files (and treated as direct file I/O)
- [ ] HttpClient is obtained from the ambient factory (design-time registration)
- [ ] Async HTTP operations are blocked on at the type provider boundary (using `Async.RunSynchronously` or equivalent)
- [ ] Response streams are disposed after content is read

**Dependencies:** cavekit-http-client-infrastructure (R1, R3)

### R2: Cache Downloaded Documents on Disk
**Description:** Recently downloaded documents must be cached in the temporary file system to avoid re-downloading identical resources across multiple type provider invocations.

**Acceptance Criteria:**
- [ ] First download of a URI writes full content to a temp file with a cache key derived from the URI
- [ ] Second request for the same URI loads content from cached file (no network call)
- [ ] Cache key is derived from the complete URI (scheme, host, path, query all included)
- [ ] Cache can be explicitly bypassed by passing a refresh flag
- [ ] Cached files are stored in the system temp directory (e.g., Path.GetTempPath())
- [ ] Cache entry age is not checked (cache is assumed valid until manually refreshed)
- [ ] If cache write fails (disk full, permission error), the operation logs a warning and falls back to non-cached fetch
- [ ] Cache read failure falls back to re-fetching from network
- [ ] XDocument parsing is performed after cache hit/fetch (same result either way)

**Dependencies:** R1 (fetch must work), cavekit-http-client-infrastructure

### R3: Support Multipart MIME Responses
**Description:** SOAP responses from X-Road meta-services may be multipart/related, containing a primary XML body and binary attachments (SwA/MTOM format). This requirement covers parsing such responses during design-time meta-service calls.

**Acceptance Criteria:**
- [ ] Content-Type header is parsed to detect multipart/related boundary marker
- [ ] Single-part responses (non-multipart) are handled without error
- [ ] If multipart, the boundary marker is extracted from Content-Type header
- [ ] Response body and attachment parts are separated using the boundary marker
- [ ] Content-Transfer-Encoding header is parsed for each part (base64, quoted-printable, 7bit, 8bit, binary)
- [ ] base64-encoded attachment content is decoded to bytes
- [ ] Other encodings are passed through without decoding
- [ ] Each attachment part includes a Content-ID header for identification
- [ ] Multipart parsing fails with a clear error if the boundary marker is not found in response
- [ ] Multipart parsing fails with a clear error if the stream ends unexpectedly mid-part
- [ ] Multipart parsing fails with a clear error if an unknown Content-Transfer-Encoding is encountered
- [ ] Parsed attachments are returned alongside the body part for downstream consumption
- [ ] Size limits (see R8) are enforced during multipart parsing

**Dependencies:** R1 (receive response), cavekit-http-client-infrastructure

### R4: Dispose Streams and Resources Properly
**Description:** Streams, file handles, and response objects must be disposed after use to avoid resource leaks, file locks, and socket exhaustion.

**Acceptance Criteria:**
- [ ] File streams opened for cache reads are disposed after XDocument content is fully read
- [ ] HttpResponseMessage objects are disposed after content is consumed
- [ ] Response content streams are disposed after reading
- [ ] Temp file streams (for cache writes) are flushed and closed
- [ ] No resource leaks when exceptions occur during parsing or network operations
- [ ] using() statements or equivalent disposal patterns are used throughout
- [ ] Tests verify that resource leaks do not occur across multiple downloads

**Dependencies:** R1 (fetch), R2 (cache), R3 (multipart)

### R5: Parse Meta-Service Responses
**Description:** Design-time type discovery relies on calling X-Road meta-services (listClients, listCentralServices, listMethods) and parsing the SOAP responses to enumerate services and methods.

**Acceptance Criteria:**
- [ ] listClients SOAP response is parsed to extract member classes, members, and subsystems
- [ ] listCentralServices SOAP response is parsed to extract central service codes
- [ ] listMethods SOAP response is parsed to extract service method definitions
- [ ] SOAP Fault responses are detected (via Fault element in Body) and raised as exceptions with fault code and fault string
- [ ] Parsed results are returned as strongly-typed structures (lists of members, services, methods, etc.)
- [ ] Empty results (no members, no services, no methods) are handled without error
- [ ] Response parsing errors (malformed XML, missing expected elements) produce clear error messages

**Dependencies:** R1 (HTTP POST for meta-service call), R3 (multipart response), R4 (resource disposal)

### R6: Block on Async HTTP at Type Provider Boundary
**Description:** The F# type provider SDK is synchronous at its entry points. Design-time HTTP operations are implemented using async (for HttpClient), but must be blocked on to maintain synchronous type provider semantics.

**Acceptance Criteria:**
- [ ] All async HTTP operations (fetch, parse, meta-service call) are internally async
- [ ] Type provider methods (static parameters, type discovery) block on async operations before returning
- [ ] Blocking is implemented using `Async.RunSynchronously` or equivalent (not thread.Join or Sleep)
- [ ] Cancellation tokens are not passed to blocking waits (type provider has no cancellation context)
- [ ] Timeout is configured on the HttpClient, not as a separate cancellation token
- [ ] No async propagation into type provider SDK (SDK remains purely synchronous)

**Dependencies:** cavekit-http-client-infrastructure (blocking uses factory)

### R7: Validate Response Status and Handle HTTP Errors
**Description:** HTTP responses with error status codes must be detected and surfaced as clear, actionable exceptions at the type provider level.

**Acceptance Criteria:**
- [ ] HTTP 2xx responses are accepted as success
- [ ] HTTP 3xx redirect responses follow HttpClient defaults (not controlled by this kit)
- [ ] HTTP 4xx client errors (404, 403, 400, etc.) produce exceptions with status code and response body snippet
- [ ] HTTP 5xx server errors (500, 503, etc.) produce exceptions with status code and reason
- [ ] Timeout errors (connection timeout, read timeout) produce clear timeout exception messages
- [ ] DNS resolution errors produce clear error messages
- [ ] SSL/TLS errors (certificate validation, handshake) produce clear error messages mentioning certificate issue
- [ ] Errors are surfaced to the type provider diagnostics (shown as red squiggles in IDE or build error)

**Dependencies:** R1 (HTTP fetch), cavekit-http-client-infrastructure (certificate validation)

### R8: SSRF (Server-Side Request Forgery) Protection
**Description:** When resolving schema imports (xsd:import elements), the system must restrict network access to prevent SSRF attacks where a malicious schema could redirect the type provider to fetch from unintended servers.

**Acceptance Criteria:**
- [ ] Only HTTPS URLs are allowed for remote schema imports (HTTP is rejected)
- [ ] Only same-origin imports are allowed (origin = scheme + host + port)
- [ ] Relative imports (e.g., `schemaLocation="./types.xsd"`) are allowed and resolved relative to the importing document's URL
- [ ] Attempts to import from disallowed origins fail with a clear error (e.g., "Import from different origin not allowed: http://...")
- [ ] File:// URLs for local imports are allowed (same-machine)
- [ ] Configuration option to whitelist additional trusted origins can be exposed (future work)

**Dependencies:** R1 (fetch), cavekit-http-client-infrastructure

### R9: Multipart Content Size Limits
**Description:** Multipart attachment parsing must enforce size limits to prevent memory exhaustion attacks or accidental fetching of very large binary payloads.

**Acceptance Criteria:**
- [ ] Individual attachment size has a configurable maximum (default: 100 MB)
- [ ] Total multipart message size has a configurable maximum (default: 500 MB)
- [ ] Size limits are enforced during streaming (not after reading entire content into memory)
- [ ] Parsing fails with a clear error if a single attachment exceeds its limit
- [ ] Parsing fails with a clear error if total message size exceeds limit
- [ ] Size limits are configurable via factory or environment variable
- [ ] Configuration is exposed in IXRoadHttpClientFactory or separate configuration class

**Dependencies:** R3 (multipart parsing), cavekit-http-client-infrastructure

## Out of Scope

- Schema validation and compilation (see cavekit-schema-parsing)
- Type generation from schemas (see cavekit-type-generation)
- Local file I/O beyond cache management and temp file cleanup
- Client certificate authentication configuration (see cavekit-core-types, runtime endpoint configuration)
- HTTP compression (gzip, deflate) — HttpClient handles this transparently
- Proxy configuration or bypass rules
- DNS resolution or host resolution strategy
- HTTP/2 or HTTP/3 protocol negotiation (use HttpClient defaults)
- HTTP response caching policy (ETags, Cache-Control headers) — all downloads are treated as fresh
- Request tracing or instrumentation
- Redirect loop detection (rely on HttpClient defaults)

## Cross-References

- **cavekit-http-client-infrastructure**: Provides the HttpClient factory and secure TLS configuration
- **cavekit-schema-parsing**: Consumes downloaded WSDL/XSD documents and parses them into schema structures
- **cavekit-type-generation**: Consumes parsed schemas and generates ProvidedTypes
- **cavekit-async-runtime**: Similar HTTP operations at runtime (async-based, with Task return types)
- **cavekit-type-provider**: Entry points that trigger design-time HTTP operations

## Source Traceability

**Current code to be replaced:**
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Http.fs` (358 lines)
  - `getXDocument`: Fetch and cache WSDL/XSD
  - `downloadXml`: Download and parse XML
  - `getServiceListFromWsdl`: Fetch and parse listMethods response
  - `getServiceListFromProducer`: Fetch and parse listClients response
  - `getServiceListFromCentralServices`: Fetch and parse listCentralServices response

**Related files:**
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Wsdl.fs` — consumes downloaded documents
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Schema.fs` — consumes downloaded schemas and performs imports
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.DesignTime.fs` — type provider entry points

**New files to be created:**
- Updated: `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Http.fs` — refactored to use HttpClient factory and async/blocking pattern
- Updated: `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.DesignTime.fs` — register design-time factory before type provider runs (if custom factory needed)

**Replaced patterns:**
- `HttpWebRequest` (REMOVED — use HttpClient)
- `ServicePointManager.SecurityProtocol` mutation (REMOVED)
- `fun _ _ _ _ -> true` certificate callback (REMOVED — use factory defaults)
- Synchronous `GetResponse()` (REPLACED with async HttpClient.SendAsync)
- Manual multipart parsing (REFACTORED into R3 requirements)
