---
created: "2026-04-16T00:00:00Z"
last_edited: "2026-04-16T00:00:00Z"
---

# Cavekit: Core Types & Contracts

## Scope

This cavekit covers the fundamental types and contracts that are shipped to consumers in the runtime assembly. It includes:
- X-Road identifier types (XRoadMemberIdentifier, XRoadServiceIdentifier, XRoadCentralServiceIdentifier)
- Custom attributes for type provider–generated code (XRoadType, XRoadElement, XRoadOperation, XRoadCollection, etc.)
- SOAP header containers (XRoadHeader) and request/response context
- Binary content attachment wrappers (BinaryContent)
- Choice type helpers and optional value handling
- Meta-service WSDL bootstrapping
- Abstract endpoint declaration (connection configuration)

This cavekit does NOT include HTTP transport, serialization, SOAP protocol mechanics, or schema parsing — those are separate domains.

## Requirements

### R1: X-Road Member Identifier Type
**Description:** Represents a client identifier in the X-Road system (member or subsystem).

**Acceptance Criteria:**
- [ ] XRoadMemberIdentifier has properties: XRoadInstance, MemberClass, MemberCode, SubsystemCode
- [ ] SubsystemCode is optional (empty string means member-level, non-empty means subsystem-level)
- [ ] ToString() returns canonical string representation (XROAD_MEMBER:instance/class/code or with /subsystem)
- [ ] Parse(string) method reconstructs identifier from canonical string
- [ ] Parse fails with clear error if string format is invalid
- [ ] Identifier equality comparison works correctly (two instances with same values are equal)
- [ ] ObjectId property returns "MEMBER" or "SUBSYSTEM" based on subsystem presence

**Dependencies:** None (fundamental type)

### R2: X-Road Service Identifier Type
**Description:** Represents a service identifier in the X-Road system (specific operation).

**Acceptance Criteria:**
- [ ] XRoadServiceIdentifier has properties: Owner (XRoadMemberIdentifier), ServiceCode, ServiceVersion
- [ ] ServiceVersion is optional (empty string means unversioned)
- [ ] ToString() returns canonical string representation (XROAD_SERVICE:instance/class/code/subsystem/service/version)
- [ ] Owner property refers to the service provider (member or subsystem)
- [ ] ServiceCode is required (non-empty)
- [ ] Equality comparison works correctly
- [ ] ObjectId property returns "SERVICE"

**Dependencies:** R1 (owner identifier)

### R3: X-Road Central Service Identifier Type
**Description:** Represents a central (shared) service in the X-Road system.

**Acceptance Criteria:**
- [ ] XRoadCentralServiceIdentifier has properties: XRoadInstance, ServiceCode
- [ ] ToString() returns canonical string representation (CENTRALSERVICE:instance/code)
- [ ] Parse(string) method reconstructs identifier from canonical string
- [ ] Parse fails with clear error if string format is invalid
- [ ] ObjectId property returns "CENTRALSERVICE"

**Dependencies:** None (fundamental type)

### R4: SOAP Header Container Type
**Description:** XRoadHeader encapsulates X-Road-specific SOAP header fields sent with every request.

**Acceptance Criteria:**
- [ ] XRoadHeader has properties for protocol version, request ID, service identifier, client identifier
- [ ] Properties are exposed for read and write
- [ ] Default values are sensible (UUID for request ID, etc.)
- [ ] Can be constructed with specific values
- [ ] ToString() provides a readable representation

**Dependencies:** R1, R2 (identifier types)

**Status:** [GAP] Current code: XRoadHeader properties are fully mutable, breaking immutability conventions.

### R5: Binary Content Attachment Type
**Description:** Wraps binary content that can be included in SOAP messages as MTOM or SwA attachments.

**Acceptance Criteria:**
- [ ] BinaryContent has ContentID property (identifier in multipart message)
- [ ] BinaryContent has stream/array access to binary data
- [ ] BinaryContent can be created from byte array
- [ ] BinaryContent can be created from stream
- [ ] Content stream is properly disposed when done
- [ ] ContentID defaults to a generated UUID if not provided
- [ ] BinaryContent can be used in generated types for binary properties

**Dependencies:** None (utility type)

**Status:** [GAP] Current code: Uses `Unchecked.defaultof<string>` for ContentId in some paths.

### R6: Custom Attributes for Type Provider Integration
**Description:** Generated code uses custom attributes to guide serialization behavior.

**Acceptance Criteria:**
- [ ] [XRoadType(LayoutKind)] attribute indicates type composition (Sequence, Choice, All)
- [ ] [XRoadElement(Name, Namespace, IsNullable, MinOccurs, MaxOccurs)] attributes on properties
- [ ] [XRoadOperation(ServiceCode, ServiceVersion)] attributes on service methods
- [ ] [XRoadRequest(ElementName, Namespace)] attributes on request message wrappers
- [ ] [XRoadResponse(ElementName, Namespace)] attributes on response message wrappers
- [ ] [XRoadCollection(MinOccurs, MaxOccurs)] attributes on repeated element properties
- [ ] Attributes are read by serialization code (cavekit-serialization, cavekit-protocol)
- [ ] Default attribute values are sensible

**Dependencies:** None (metadata)

### R7: Choice Type Helpers and Interfaces
**Description:** Choice types (XSD choice elements, WSDL unions) require special handling.

**Acceptance Criteria:**
- [ ] Choice type interface hierarchy is predefined (up to 8 union cases)
- [ ] Each union case is a discriminated union case with optional data
- [ ] Choice interface has method to extract union case value
- [ ] Choice wrapper types implement correct interface
- [ ] Generated choice types compile without null-reference errors
- [ ] Serialization correctly identifies which union case is active

**Dependencies:** R6 (attributes guide choice creation)

**Status:** [GAP] Current code: Choice interfaces hardcoded to 8 alternatives. No extension path for larger unions.

### R8: Optional Value Helpers
**Description:** Optional values (nullable, minOccurs=0) use .NET nullable types or Option.

**Acceptance Criteria:**
- [ ] Nullable<T> is used for value types (int?, DateTime?)
- [ ] Option<T> is used for reference types (string option, object option)
- [ ] Default values for optional properties are None/null
- [ ] Serialization handles None/null correctly
- [ ] Deserialization creates Option/Nullable correctly
- [ ] No unwanted null dereferences in generated code

**Dependencies:** None (utility)

### R9: Abstract Endpoint Declaration
**Description:** Provides configuration and behavior for service endpoints (connection details, certificates, events).

**Acceptance Criteria:**
- [ ] AbstractEndpointDeclaration has Uri property (security server or service endpoint)
- [ ] Has AuthenticationCertificates list for client authentication
- [ ] Has AcceptedServerCertificate for pinned server certificate validation
- [ ] Has DefaultOffset for date/time deserialization context
- [ ] Supports ResponseReady event (raised after response received, before deserialization)
- [ ] Event includes response metadata (request ID, service code, version)
- [ ] Configuration is accessible to serialization and protocol layers

**Dependencies:** R1, R2 (identifier types), cavekit-protocol

### R10: Meta-Service WSDL Bootstrapping [GAP]
**Description:** X-Road meta-services (listMethods, listClients) are accessed via SOAP but their WSDL is not typically provided explicitly.

**Acceptance Criteria:**
- [ ] MetaServices module provides WSDL definitions for listMethods operation
- [ ] MetaServices module provides type definitions for producer list and service list
- [ ] WSDL can be fetched without requiring a WSDL URL parameter
- [ ] Meta-service calls are formatted correctly for X-Road v6

**Dependencies:** R1, R2 (identifier types)

**Status:** [GAP] Current code: `MetaServices.openWsdlStream` opens stream without try/finally. Resource leak on failure (line 44).

### R11: Identifier Type Parsing Robustness [GAP]
**Description:** Identifier parsing from strings should be resilient and provide good error messages.

**Acceptance Criteria:**
- [ ] Parse methods use Result<T, Error> or similar, not exceptions
- [ ] Error messages suggest correct format and provide examples
- [ ] Partial parsing failures (valid prefix, invalid suffix) are reported clearly
- [ ] Whitespace handling is correct (trimmed or rejected consistently)
- [ ] Case sensitivity is correct (X-Road codes are case-sensitive)

**Dependencies:** R1, R2, R3 (identifier types)

**Status:** [GAP] Current code: Parse methods throw with `failwith`. No Result-based error handling. Lines 85-91 in FSharp.Data.XRoad.fs.

### R12: Request Context and Tracing
**Description:** Requests can be correlated and traced through the system.

**Acceptance Criteria:**
- [ ] XRoadRequest has RequestId (UUID, unique per request)
- [ ] XRoadResponse includes request metadata for correlation
- [ ] Request ID is visible to ResponseReady event subscribers
- [ ] Service code and version are available in ResponseReady event
- [ ] Allows correlation between request and response for async scenarios

**Dependencies:** R4, R9

## Out of Scope

- Protocol version negotiation (fixed at 4.0)
- Endpoint URL path handling (full URL is provided)
- Certificate revocation checking (delegated to OS)
- Request/response logging infrastructure (application-level concern)
- Configuration file parsing (application-level)
- Dependency injection or service registration

## Cross-References

- **cavekit-serialization**: Reads custom attributes on generated types
- **cavekit-protocol**: Uses XRoadHeader, AbstractEndpointDeclaration, identifier types
- **cavekit-type-generation**: Generates types decorated with custom attributes
- **cavekit-http-transport**: Uses identifier types in meta-service calls
- **cavekit-type-provider**: Constructs identifier types for service discovery

## Source Traceability

**Source files:**
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.fs` (543 lines) — Identifier types, XRoadHeader, core contract definitions
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Attributes.fs` (194 lines) — Custom attributes for type provider integration
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Choices.fs` (54 lines) — Choice type helpers and interface definitions
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.MetaServices.fs` (44 lines) — Meta-service WSDL bootstrapping

**Design Notes:**
- All identifier types are shipped in the runtime assembly, so users see them as public API
- Custom attributes guide serialization (read by cavekit-serialization at runtime)
- XRoadHeader is mutable by design but breaks immutability convention (see note on R4)
- Choice and Optional helpers are utility types for generated code, not typically used directly by users
- AbstractEndpointDeclaration is the main configuration point for service connections
