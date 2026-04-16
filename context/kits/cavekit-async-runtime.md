---
created: "2026-04-17T00:00:00Z"
last_edited: "2026-04-17T00:00:00Z"
---

# Cavekit: Async Runtime API

## Scope

This cavekit covers the runtime service call execution model and the breaking-change migration from synchronous to asynchronous APIs. It includes:
- Async request/response execution using HttpClient and Task<T>
- Generated service port method signatures (Task-based)
- SOAP envelope construction and serialization
- HTTP POST request execution with proper header setup
- Response parsing and SOAP fault handling
- Multipart response handling for attachments
- Deserialization of responses to CLR objects
- Cancellation token support throughout
- Resource disposal (HttpResponseMessage, streams)
- RequestReady and ResponseReady event handling in async context
- Abstract endpoint declaration with injected HttpClient factory
- Optional [Obsolete] synchronous wrapper for migration period

This cavekit does NOT include the HTTP client factory abstraction (see cavekit-http-client-infrastructure), IL-based serialization delegate generation (see cavekit-serialization), or WSDL/schema parsing (see cavekit-schema-parsing). It also does NOT include design-time operations — see cavekit-design-time-http for those.

## Requirements

### R1: Async Service Call API
**Description:** All runtime service calls must be asynchronous and return `Task<ResponseType>` instead of synchronous `ResponseType`. This is a breaking change from the current synchronous API.

**Acceptance Criteria:**
- [ ] Generated service port methods have signature `Task<ResponseType> MethodNameAsync(params...) : Task<ResponseType>`
- [ ] Service method names are suffixed with "Async" to indicate async behavior
- [ ] Method returns a Task that completes when the HTTP response is received and deserialized
- [ ] Awaiting the task blocks the caller until the service response is complete
- [ ] Exceptions (SOAP faults, HTTP errors, deserialization failures) are propagated as task exceptions
- [ ] Multiple concurrent service calls can be made in parallel (no artificial serialization)
- [ ] Documentation warns that this is a breaking change from prior synchronous API

**Dependencies:** None (foundational API change)

### R2: Cancellation Token Support
**Description:** Async service calls must support cancellation via CancellationToken, allowing callers to cancel long-running requests.

**Acceptance Criteria:**
- [ ] Service port methods accept optional `CancellationToken` parameter
- [ ] Parameter can be omitted (defaults to `CancellationToken.None`)
- [ ] Cancellation token is passed to HttpClient.SendAsync() call
- [ ] Cancellation token is passed to response content reading operations
- [ ] If cancellation is requested, the task throws `OperationCanceledException`
- [ ] HTTP connection is cleaned up immediately upon cancellation (not left hanging)
- [ ] CancellationToken can be used to implement request timeouts (caller creates token with timeout)

**Dependencies:** R1 (async API)

### R3: AbstractEndpointDeclaration HttpClient Factory Injection
**Description:** Service endpoint instances must expose an injectable HttpClient factory, enabling consumers to customize HTTP configuration or mock HttpClient for testing.

**Acceptance Criteria:**
- [ ] AbstractEndpointDeclaration (or mixin type) exposes `HttpClientFactory` property (type: `IXRoadHttpClientFactory`)
- [ ] Property can be set via constructor parameter or property setter
- [ ] Default value is the ambient/global default factory if not set
- [ ] Factory is called once per service method invocation to obtain an HttpClient instance
- [ ] Each endpoint instance can have a different factory (enables per-service configuration)
- [ ] Runtime execution code receives the HttpClient from the factory before making HTTP calls
- [ ] Factory is used consistently across all methods of the endpoint

**Dependencies:** cavekit-http-client-infrastructure (R1, R4), cavekit-core-types (AbstractEndpointDeclaration)

### R4: HTTP Response Resource Disposal
**Description:** HttpResponseMessage and all associated streams must be properly disposed to prevent socket exhaustion and resource leaks.

**Acceptance Criteria:**
- [ ] HttpResponseMessage returned from SendAsync() is disposed after response content is consumed
- [ ] Response content stream is disposed after reading
- [ ] If deserialization throws an exception, the response is still disposed (use try-finally or using statement)
- [ ] Response body is read completely or disposed without reading (no dangling streams)
- [ ] Multipart response parts are disposed after attachment extraction
- [ ] No resource leaks are observable when repeating service calls in a loop

**Dependencies:** R1 (async API)

### R5: SOAP Fault Detection and Exception Handling
**Description:** SOAP Fault responses must be detected and converted to exceptions with proper fault code and message preservation.

**Acceptance Criteria:**
- [ ] Response XML body is checked for SOAP Fault element (in SOAP 1.1 namespace)
- [ ] If Fault is found: faultCode, faultstring, and faultactor elements are extracted
- [ ] Exception type `XRoadFaultException` (or similar) is raised with fault code and message
- [ ] Fault exception includes original fault XML for detailed inspection if needed
- [ ] Fault exceptions are raised synchronously from the async task (not swallowed or wrapped)
- [ ] Non-fault responses (normal service responses) pass through without error
- [ ] HTTP error status codes (4xx, 5xx) are treated as HTTP errors, not SOAP faults (separate exception path)

**Dependencies:** R1 (async API), cavekit-protocol (SOAP fault semantics)

### R6: Configurable Request Timeout
**Description:** Service calls must support configurable timeout to prevent indefinite hangs on slow or unresponsive servers.

**Acceptance Criteria:**
- [ ] HttpClient.Timeout is set to a default reasonable value (e.g., 30 seconds) in the factory
- [ ] Timeout is configurable per factory instance (factory can be set to different timeouts)
- [ ] Timeout applies to the entire request/response cycle (connection, sending, reading response)
- [ ] If timeout is exceeded, HttpRequestException with timeout message is raised
- [ ] Callers can override timeout per-request by using CancellationTokenSource with timeout
- [ ] Timeout value is exposed in documentation/configuration

**Dependencies:** cavekit-http-client-infrastructure (R2, factory configuration), R2 (cancellation)

### R7: Async WSDL Bootstrapping (MetaServices)
**Description:** Initial WSDL bootstrapping (fetching service lists from security server meta-services) must become async to enable non-blocking initialization.

**Acceptance Criteria:**
- [ ] MetaServices functions for fetching listClients, listCentralServices, listMethods become async
- [ ] These functions return `Task<...>` instead of synchronous types
- [ ] Async meta-service calls use the injected HttpClient factory (from AbstractEndpointDeclaration)
- [ ] Callers can await meta-service results without blocking
- [ ] MetaServices operations respect cancellation tokens
- [ ] Same SOAP fault and HTTP error handling as normal service calls

**Dependencies:** R1 (async API), R3 (factory injection), cavekit-core-types (MetaServices API)

### R8: Synchronous Compatibility Wrapper (Obsolete)
**Description:** To ease migration from the old synchronous API, an optional synchronous wrapper may be provided, but marked as obsolete and discouraged.

**Acceptance Criteria:**
- [ ] Synchronous wrapper method is provided (e.g., `MethodName(params...)` wrapping `MethodNameAsync`)
- [ ] Wrapper is marked with `[Obsolete("Use MethodNameAsync instead")]` attribute
- [ ] Wrapper blocks on the async task internally (using `Task.Wait()` or similar)
- [ ] Wrapper preserves exception behavior (task exceptions are re-raised synchronously)
- [ ] Deprecation message directs users to migrate to async API
- [ ] Wrapper is provided only if backward compatibility is a requirement (not mandatory for this kit)

**Dependencies:** R1 (async API), R2 (cancellation), R3 (factory injection)

### R9: Event Compatibility in Async Context
**Description:** RequestReady and ResponseReady events (from AbstractEndpointDeclaration) must fire at appropriate times within the async execution flow and maintain correct synchronization context.

**Acceptance Criteria:**
- [ ] RequestReady event fires before the HTTP request is sent (after SOAP envelope construction)
- [ ] ResponseReady event fires after the HTTP response is received (before deserialization)
- [ ] Events fire on the calling synchronization context (if applicable) — use ConfigureAwait(false) if needed
- [ ] Events can inspect the request envelope or raw response if needed
- [ ] Events do not block the async flow (subscribers must be async-safe or very fast)
- [ ] Event handler exceptions are propagated to the caller (not swallowed)

**Dependencies:** R1 (async API), cavekit-core-types (event definitions)

### R10: SOAP Envelope Construction
**Description:** SOAP 1.1 envelopes with X-Road v6 headers must be constructed before sending requests.

**Acceptance Criteria:**
- [ ] Envelope element uses correct SOAP 1.1 namespace
- [ ] Envelope wraps Header and Body elements
- [ ] Header contains X-Road v6 elements: protocolVersion ("4.0"), id (UUID), service, client
- [ ] Body element contains the serialized request message (operation input)
- [ ] Namespace declarations for X-Road are correct (xro maps to http://x-road.eu/xsd/xroad.xsd, iden maps to http://x-road.eu/xsd/identifiers)
- [ ] SOAP envelope parser is hardened against XXE injection (external entities are disabled when reading any XML)
- [ ] Encoding is UTF-8
- [ ] Envelope is serialized to a stream for transmission
- [ ] Service version and subsystem code are optional (omitted if empty)

**Dependencies:** cavekit-serialization (R1, envelope serialization), cavekit-core-types (identifier types)

### R11: HTTP POST Request Execution
**Description:** SOAP envelopes must be sent as HTTP POST requests with correct headers and certificate configuration.

**Acceptance Criteria:**
- [ ] HTTP POST method is used (not GET, PUT, DELETE)
- [ ] Request URL is the X-Road security server endpoint (from endpoint configuration)
- [ ] Content-Type header is set to "text/xml; charset=utf-8"
- [ ] SOAPAction header is set to empty string
- [ ] Request body is the serialized SOAP envelope
- [ ] Client certificates (if configured on endpoint) are attached to the request
- [ ] Request is sent using HttpClient.SendAsync() (async, not synchronous GetResponse)
- [ ] Response is received as HttpResponseMessage with status code and headers
- [ ] HTTP response body is read as stream for multipart parsing or direct deserialization

**Dependencies:** R10 (envelope), cavekit-http-client-infrastructure (HttpClient/certificates), R2 (cancellation)

### R12: Multipart Response Handling
**Description:** Responses with attachments (multipart/related) must be parsed to separate the body from attachments for use during deserialization.

**Acceptance Criteria:**
- [ ] Content-Type header is checked for multipart/related indicator
- [ ] Single-part responses are handled without error
- [ ] Multipart parsing extracts the primary body part and attachment parts
- [ ] Attachment parts are stored by Content-ID for retrieval during deserialization
- [ ] Attachments are available to the deserialization layer (MTOM/SwA handlers)
- [ ] Multipart parsing is performed by underlying HTTP transport layer (cavekit-design-time-http R3 equivalent)
- [ ] Size limits are enforced during multipart parsing

**Dependencies:** cavekit-design-time-http (R3 equivalent), R11 (HTTP response)

### R13: Response Deserialization
**Description:** Parsed response XML is deserialized to CLR response objects using generated IL-based delegates.

**Acceptance Criteria:**
- [ ] Response body XML element is passed to the operation's deserializer delegate
- [ ] Deserializer constructs response object with properly typed properties
- [ ] Attachments (from multipart response) are available to deserializer if needed
- [ ] Deserialization errors (type conversion, missing required elements) produce clear exceptions
- [ ] Deserialized response object is returned to the caller
- [ ] Response is fully deserialized before the task completes

**Dependencies:** cavekit-serialization (R2, deserializer delegates), R12 (multipart), R11 (response received)

## Out of Scope

- IL-based serializer/deserializer code generation (see cavekit-serialization)
- WSDL/schema parsing and type discovery (see cavekit-schema-parsing, cavekit-type-generation)
- Type provider infrastructure and compile-time type generation (see cavekit-type-provider)
- Client certificate provisioning or lifecycle management (see cavekit-core-types)
- HTTP compression (transparent to this kit)
- Request/response logging or instrumentation
- Proxy configuration
- DNS resolution
- HTTP redirect handling (use HttpClient defaults)
- Automatic retry on transient failures
- Circuit breaker or bulkhead patterns
- Request correlation or tracing IDs

## Cross-References

- **cavekit-http-client-infrastructure**: Provides HttpClient factory, secure TLS configuration, certificate pinning
- **cavekit-design-time-http**: Design-time equivalent operations (blocking on async, WSDL fetch, meta-service calls)
- **cavekit-serialization**: Generates IL-based serializer and deserializer delegates used by this kit
- **cavekit-core-types**: AbstractEndpointDeclaration, identifier types, event definitions
- **cavekit-protocol** (existing): Provides SOAP 1.1 envelope semantics and fault handling (may be incorporated into this kit or kept separate)
- **cavekit-type-generation**: Generates service port class methods that call this runtime layer

## Source Traceability

**Current code to be replaced:**
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Protocol.fs` (323 lines) — synchronous request/response handling
  - `XRoadRequest.SendMessage`: Synchronous HTTP POST, returns response
  - `XRoadResponse.RetrieveMessage`: Synchronous response parsing
  - Multipart handling for attachments
  - SOAP fault detection
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Runtime.fs` (94 lines) — service call execution
  - `MakeServiceCall`: Synchronous wrapper that combines protocol operations
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.MetaServices.fs` (153 lines) — synchronous meta-service calls

**Files to be created/updated:**
- New: `src/FSharp.Data.XRoad/FSharp.Data.XRoad.AsyncProtocol.fs` (or refactored Protocol.fs) — async SOAP protocol operations
- Updated: `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Runtime.fs` — `MakeServiceCallAsync` (primary), optional sync wrapper
- Updated: `src/FSharp.Data.XRoad/FSharp.Data.XRoad.MetaServices.fs` — async meta-service functions
- Updated: `src/FSharp.Data.XRoad/FSharp.Data.XRoad.fs` — exports async API
- Updated: Generated service port type templates — generated methods return Task<T>

**Replaced patterns:**
- `HttpWebRequest.GetResponse()` (REMOVED — use HttpClient.SendAsync())
- Synchronous task blocking in generated code (REMOVED — generated methods are async)
- `ServicePointManager.SecurityProtocol` mutation (REMOVED — handled by factory)
- `fun _ _ _ _ -> true` certificate validation (REMOVED — use factory validation)
- `Request.Timeout` property (REPLACED with HttpClient.Timeout in factory)
