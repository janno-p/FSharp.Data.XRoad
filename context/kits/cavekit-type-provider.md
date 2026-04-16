---
created: "2026-04-16T00:00:00Z"
last_edited: "2026-04-16T00:00:00Z"
---

# Cavekit: Type Provider Infrastructure

## Scope

This cavekit covers the F# type provider mechanisms that discover services on X-Road security servers and generate typed service clients. It includes:
- Type provider entry points (XRoadInstanceProvider, XRoadProducerProvider)
- Static parameter handling (URI, security server connection, filter options)
- Type discovery from security server meta-services
- Type caching and invalidation
- Error reporting via ProvidedTypes framework
- X-Road service identifier types (nested hierarchy: Instance → Member → Subsystem → Service)
- Central service enumeration

This cavekit does NOT include WSDL/schema fetching (cavekit-design-time-http), schema parsing (cavekit-schema-parsing), or code generation logic (cavekit-type-generation).

## Requirements

### R1: Implement XRoadInstanceProvider Type Provider
**Description:** The type provider entry point that allows users to enumerate X-Road instances and services.

**Acceptance Criteria:**
- [ ] Type provider is registered with TypeProviderAttribute
- [ ] Accepts static parameters: security server URI, client identifier, optional filters
- [ ] Generates a type hierarchy representing members, subsystems, and services
- [ ] Type discovery is lazy (computed on first access)
- [ ] Type cache is populated as types are generated

**Dependencies:** cavekit-design-time-http (R5, R6, R7), cavekit-type-generation (R1)

### R2: Discover X-Road Members and Member Classes
**Description:** Enumerate all members (organizations, subsystems) on a security server.

**Acceptance Criteria:**
- [ ] listClients meta-service is called with the X-Road instance name
- [ ] Member classes are enumerated (e.g., "GOV", "COM", "ORG")
- [ ] For each member class, all members are listed with display name and code
- [ ] Members are presented as nested types (Member → Services, Member → Subsystems)
- [ ] Subsystems of a member are available as nested types
- [ ] Services available to a member are enumerable
- [ ] Member list is sorted by name for predictable ordering
- [ ] Errors in meta-service calls are reported without crashing the type provider

**Dependencies:** cavekit-design-time-http (R5)

### R3: Discover and Navigate Subsystems
**Description:** Within a member, enumerate all registered subsystems.

**Acceptance Criteria:**
- [ ] Subsystems are discovered as part of member enumeration
- [ ] Each subsystem is presented as a nested type with subsystem code as name
- [ ] Services belonging to a subsystem are enumerable
- [ ] Subsystem information (code, associated member) is accessible

**Dependencies:** R2 (member discovery)

### R4: Enumerate Service Methods
**Description:** For a selected service (subsystem or member), list available operations.

**Acceptance Criteria:**
- [ ] listMethods meta-service is called for a given service
- [ ] Service methods are returned as typed operations with XRoadServiceIdentifier
- [ ] Each method is a ProvidedMethod generating a client invocation
- [ ] Method names and versions are extracted from service identifier
- [ ] Errors in meta-service calls are surfaced as note fields (temporary workaround)

**Dependencies:** cavekit-design-time-http (R7), cavekit-type-generation (R6)

### R5: Implement XRoadProducerProvider
**Description:** Alternative type provider entry point for direct WSDL URL-based client generation.

**Acceptance Criteria:**
- [ ] Type provider accepts a WSDL URL as static parameter
- [ ] WSDL is fetched and parsed
- [ ] Service operations are extracted from WSDL
- [ ] Service client type is generated with all operations
- [ ] Same type generation logic as XRoadInstanceProvider

**Dependencies:** cavekit-design-time-http (R1), cavekit-schema-parsing, cavekit-type-generation

### R6: Handle Static Parameters
**Description:** Type providers accept configuration via static parameters passed at compile time.

**Acceptance Criteria:**
- [ ] Security server URI is required and validated
- [ ] X-Road instance name is required
- [ ] Client identifier (member code) is required
- [ ] Optional filters (comma-separated operation names) are supported
- [ ] Operation name filters include only matching methods
- [ ] Parameters are documented with XML doc comments
- [ ] Invalid parameters fail with clear error messages at compile time

**Dependencies:** None (configuration)

### R7: Cache Generated Types [GAP]
**Description:** Type generation is expensive; previously generated types should be cached.

**Acceptance Criteria:**
- [ ] Generated types are cached in memory during a single compilation session
- [ ] Cache key is derived from security server URI, client ID, service ID, and filter options
- [ ] Cache hit avoids re-fetching and re-parsing WSDL
- [ ] Cache is per-compilation session (invalidated between runs)
- [ ] Cache key collision is impossible (SHA256 or deterministic hash)
- [ ] Cache does not grow unbounded (LRU or max size enforcement)

**Dependencies:** None (optimization)

**Status:** [GAP] Current code: Cache uses SHA1 hash (cryptographically weak). Cache never expires. Keyed on URI only, not full configuration.

### R8: Error Reporting and Diagnostics [GAP]
**Description:** Errors in type provider execution must be reported clearly to the user.

**Acceptance Criteria:**
- [ ] Errors in meta-service calls are reported as compile-time diagnostics
- [ ] Errors in WSDL parsing are reported with location information
- [ ] Network errors are reported with attempted URI and timeout information
- [ ] Certificate validation errors are reported clearly
- [ ] Errors do not cause the entire type provider to fail; best-effort typing is provided
- [ ] Error messages are actionable (suggest retry, check credentials, etc.)
- [ ] ProvidedTypes note fields are avoided in favor of proper diagnostics

**Dependencies:** None (error handling)

**Status:** [GAP] Current code: Errors surfaced as note field members (line 75: `[e.ToString() |> createNoteField]`). No proper diagnostic API usage.

### R9: Support Asynchronous Type Discovery [GAP]
**Description:** Type provider operations (network calls) should not block the compiler.

**Acceptance Criteria:**
- [ ] Type discovery supports timeouts
- [ ] Type discovery supports cancellation tokens
- [ ] Long-running operations can be cancelled from the IDE
- [ ] Meta-service calls support cancellation
- [ ] WSDL fetch and parsing support cancellation

**Dependencies:** cavekit-design-time-http

**Status:** [GAP] Current code: No cancellation support. No timeout on `request.GetResponse()`.

### R10: Support Central Services Discovery
**Description:** Central services (shared X-Road services) must be discoverable and enumerable.

**Acceptance Criteria:**
- [ ] listCentralServices meta-service is called to enumerate central services
- [ ] Central services are presented as top-level services (not per-member)
- [ ] Central service identifier is properly formed (CENTRALSERVICE:instance/code)
- [ ] Central services are included in service enumeration

**Dependencies:** cavekit-design-time-http (R6), cavekit-type-generation

### R11: Validate Type Provider Configuration [GAP]
**Description:** Invalid configurations must be detected early and reported.

**Acceptance Criteria:**
- [ ] Security server URI format is validated (https required, valid hostname)
- [ ] Client identifier format is validated (correct X-Road namespace format)
- [ ] Service identifier format is validated
- [ ] X-Road instance name is validated (non-empty, valid characters)
- [ ] Invalid configurations fail at type provider initialization time
- [ ] Validation errors are reported with corrective guidance

**Dependencies:** None (validation)

**Status:** [GAP] Current code: No validation of static parameters. Invalid URLs can cause null dereferences at parse time.

### R12: Support Multi-Version Services
**Description:** X-Road services can have multiple versions; all versions must be discoverable.

**Acceptance Criteria:**
- [ ] Service versions are extracted from service identifier
- [ ] Each service version generates a separate type (e.g., "ServiceName/v1", "ServiceName/v2")
- [ ] Version-specific operations are available on each type
- [ ] Unversioned services (empty version) are handled correctly

**Dependencies:** R4 (method enumeration)

## Out of Scope

- F# compiler integration (compilation pipeline, type checking)
- ProvidedTypes SDK implementation details (quotation handling, inheritance models)
- Visual Studio integration and IntelliSense
- Type provider packaging and distribution
- Configuration file formats (appsettings.json is application-level, not provider-level)
- Security and authentication certificates (configured separately, see cavekit-protocol)

## Cross-References

- **cavekit-http-transport**: Fetches WSDL, producer lists, method lists (legacy; superseded by cavekit-design-time-http)
- **cavekit-design-time-http**: HttpClient-based WSDL/meta-service fetching (replaces cavekit-http-transport)
- **cavekit-schema-parsing**: Parses WSDL into intermediate schema representation
- **cavekit-type-generation**: Generates ProvedTypeDefinitions for services
- **cavekit-core-types**: X-Road identifier types used in type discovery

## Source Traceability

**Source files:**
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.DesignTime.fs` (327 lines) — Type provider entry points and service discovery

**Related files:**
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.MetaServices.fs` (44 lines) — Meta-service WSDL bootstrapping
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.fs` — X-Road identifier types
- `paket-files/fsprojects/FSharp.TypeProviders.SDK/src/ProvidedTypes.fs` — ProvidedTypes framework

**Typical flow:**
1. User invokes type provider with static parameters in F# code
2. Type provider fetches producer/central service list (cavekit-design-time-http, R5, R6)
3. Type hierarchy is built (R2, R3, R4, R10)
4. For each service, WSDL is fetched and parsed (cavekit-design-time-http, cavekit-schema-parsing)
5. Types are generated from schema (cavekit-type-generation)
6. Generated types are cached and exposed to compiler
