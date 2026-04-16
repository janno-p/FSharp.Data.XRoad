---
created: "2026-04-17T00:00:00Z"
last_edited: "2026-04-17T00:00:00Z"
---

# Cavekit: HTTP Client Infrastructure

## Scope

This cavekit defines the shared HTTP client abstraction layer used by both design-time (type provider) and runtime (service call) subsystems. It includes:
- Factory interface for creating configured HttpClient instances
- Default factory implementation with secure TLS settings
- Ambient/static design-time factory registration (for type providers run at compile-time)
- Runtime factory injection via endpoint constructors or properties
- Optional certificate thumbprint pinning for self-signed server certificates
- Named client support for different X-Road security servers with different configurations
- HttpClient lifecycle management (long-lived, connection pooling)

This cavekit does NOT include the actual HTTP request/response handling logic (see cavekit-design-time-http and cavekit-async-runtime for those). It also does not cover specific URL construction or multipart MIME parsing — those are delegated to the consumers.

## Requirements

### R1: Factory Interface for HttpClient Creation
**Description:** A factory interface enables both design-time and runtime to obtain pre-configured HttpClient instances without hardcoding HTTP details.

**Acceptance Criteria:**
- [ ] Factory interface `IXRoadHttpClientFactory` is defined in the runtime assembly
- [ ] Factory has a method `CreateHttpClient(name: string) : HttpClient` that returns a configured instance
- [ ] Factory optionally accepts a configuration parameter (e.g., timeout, max redirects)
- [ ] Factory is publicly exported for consumer registration
- [ ] Factory may be sync or async; design-time blocks on async if needed
- [ ] Multiple calls with the same name return equivalent (or the same) HttpClient instance

**Dependencies:** None (foundational)

### R2: Default Secure Factory Implementation
**Description:** The default factory implementation must produce HttpClient instances configured with secure TLS settings and no certificate validation bypass.

**Acceptance Criteria:**
- [ ] Default factory enables TLS 1.2 as minimum
- [ ] Default factory enables TLS 1.3 if available on the runtime
- [ ] Default factory disables TLS 1.0 and 1.1
- [ ] Default factory disables deprecated cipher suites
- [ ] Default factory disables certificate validation bypass (no callback accepting all certs)
- [ ] Default factory does NOT mutate global `ServicePointManager` state
- [ ] Default factory creates HttpClientHandler with HttpClient instance
- [ ] Default factory configures reasonable defaults for timeout (e.g., 30 seconds)
- [ ] Default factory creates HttpClient instances with connection pooling enabled
- [ ] Factory instances are thread-safe

**Dependencies:** None (foundational security)

### R3: Design-Time Ambient Factory Registration
**Description:** Type providers run at compile-time in an ambient context where constructor dependency injection is not possible. Consumers must be able to register a custom factory before type provider execution begins.

**Acceptance Criteria:**
- [ ] An ambient/static registration slot is provided (e.g., `XRoadHttpClient.SetDesignTimeFactory(factory)`)
- [ ] Registration must occur before type checking (before F# language service loads the type provider)
- [ ] Registration is thread-local or global (consumer responsibility to ensure single-threaded access during type checking)
- [ ] If no custom factory is registered, the default factory is used
- [ ] The registered factory is used for all design-time HTTP operations (WSDL fetch, schema import, meta-service calls)
- [ ] Factory registration is cleared or reset between compilation units if needed (consumer responsibility)
- [ ] Documentation explains that registration must happen early in the compilation pipeline (e.g., in a project's startup or initialization code)

**Dependencies:** R1 (interface), R2 (default factory)

### R4: Runtime Instance-Level Factory Injection
**Description:** Generated service endpoint types must allow consumers to inject a custom HttpClient factory at runtime, enabling per-service configuration or mock substitution for testing.

**Acceptance Criteria:**
- [ ] `AbstractEndpointDeclaration` base class (or mixin type) exposes an `HttpClientFactory` property
- [ ] Property is injectable via constructor or property setter
- [ ] Default value is the ambient/default factory if none provided
- [ ] Each endpoint instance can have a different factory
- [ ] Factory is called once per service method invocation to obtain an HttpClient
- [ ] HttpClient is reused across multiple calls to the same endpoint (or obtained fresh, but factory is called once per call)
- [ ] Runtime code passes the factory to internal protocol execution functions

**Dependencies:** R1 (interface), cavekit-core-types (AbstractEndpointDeclaration)

### R5: Optional Certificate Thumbprint Pinning
**Description:** For X-Road deployments using self-signed server certificates, consumers may optionally pin a specific server certificate by its thumbprint, bypassing the normal trust chain validation while still protecting against MITM attacks.

**Acceptance Criteria:**
- [ ] Factory interface accepts optional certificate thumbprint configuration (e.g., `pinCertificateThumbprint: string option`)
- [ ] If thumbprint is configured, the factory sets up a custom `HttpClientHandler.ServerCertificateCustomValidationCallback`
- [ ] Callback validates the server certificate's thumbprint against the pinned value
- [ ] If thumbprint matches, validation succeeds (certificate is trusted)
- [ ] If thumbprint does not match, validation fails (certificate is rejected)
- [ ] The callback still performs basic certificate validity checks (not expired, signature valid)
- [ ] Multiple pinned thumbprints may be configured (any match succeeds)
- [ ] If no pinned thumbprints are configured, normal trust chain validation is used
- [ ] Pinning is configured at factory construction time, not per-request

**Dependencies:** R1 (interface), R2 (default factory)

### R6: Long-Lived HttpClient Instances and Connection Pooling
**Description:** HttpClient instances must be reused across multiple requests to enable connection pooling and avoid socket exhaustion.

**Acceptance Criteria:**
- [ ] Factory should return the same HttpClient instance for repeated calls with the same name (or create long-lived instances)
- [ ] HttpClient instances are NOT disposed after each request
- [ ] Connection pooling is enabled by default in HttpClientHandler
- [ ] HttpClientHandler connection pool size can be configured (if needed)
- [ ] Factory documentation warns against creating new HttpClient instances per-request
- [ ] Tests verify that connection pooling works (repeated requests to same host reuse connections)

**Dependencies:** R2 (default factory)

## Out of Scope

- HTTP request/response header construction and handling (see cavekit-design-time-http and cavekit-async-runtime)
- URL parsing or validation
- SOAP envelope construction
- Multipart MIME boundary parsing or part extraction
- Response body deserialization
- Certificate chain validation implementation details (delegated to the TLS stack)
- Proxy configuration or DNS resolution
- HTTP/2 or HTTP/3 protocol negotiation
- Authentication schemes (client certificates configured separately in endpoint types)
- Request/response logging or instrumentation
- Compression (gzip, deflate)
- Redirect handling strategy (use HttpClientHandler defaults)

## Cross-References

- **cavekit-design-time-http**: Consumes the factory to fetch WSDL, schemas, meta-service responses at compile-time
- **cavekit-async-runtime**: Consumes the factory to send SOAP requests and receive responses at runtime
- **cavekit-core-types**: AbstractEndpointDeclaration must expose HttpClientFactory property for runtime injection
- **cavekit-protocol** (existing): Will be replaced by cavekit-async-runtime, which uses this factory

## Source Traceability

This is a **NEW domain** (greenfield). No existing cavekit or source file covers this abstraction layer. The current code in `Protocol.fs` and `Http.fs` uses `HttpWebRequest`/`ServicePointManager` directly without an abstraction.

**Files to be created:**
- New module: `FSharp.Data.XRoad/FSharp.Data.XRoad.HttpClient.fs` (or similar) — defines `IXRoadHttpClientFactory` interface and default implementation
- Updated: `FSharp.Data.XRoad/FSharp.Data.XRoad.fs` — exports factory interface; may add ambient registration helper
- Updated: `FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.DesignTime.fs` — uses the ambient factory to obtain HttpClient instances

**Replaced patterns:**
- `ServicePointManager.SecurityProtocol` global mutation (REMOVED)
- `fun _ _ _ _ -> true` certificate validation callback (REMOVED)
- Per-domain HttpWebRequest instantiation (REPLACED with factory pattern)
