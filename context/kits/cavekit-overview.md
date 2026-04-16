---
created: "2026-04-16T00:00:00Z"
last_edited: "2026-04-17T00:00:00Z"
---

# Cavekit Overview: FSharp.Data.XRoad

## Project Summary

FSharp.Data.XRoad is an F# type provider for the X-Road (Nordic/Baltic e-government) data exchange layer. It enables F# developers to consume X-Road services with full type safety at compile time. The system consists of two assemblies: a design-time type provider assembly (generates types from WSDL) and a runtime assembly (executes SOAP requests and handles serialization).

This document indexes all domain Cavekits and their relationships.

## Domain Index

| # | Domain | File | Req Count | Gaps | Status | Description |
|---|--------|------|-----------|------|--------|-------------|
| 1 | HTTP Transport & Security | `cavekit-http-transport.md` | 13 | 7 | LEGACY | HTTPS/HTTP document fetching, caching, certificate validation, multipart MIME parsing (superseded by R2-R3 kits) |
| 2 | WSDL & Schema Parsing | `cavekit-schema-parsing.md` | 11 | 3 | ACTIVE | WSDL document model, XSD type parsing, namespace handling, composition |
| 3 | Type Generation | `cavekit-type-generation.md` | 11 | 3 | ACTIVE | ProvidedTypeDefinition generation, inheritance, custom attributes, collections |
| 4 | Type Provider Infrastructure | `cavekit-type-provider.md` | 12 | 6 | ACTIVE | Type provider entry points, service discovery, static parameters, caching |
| 5 | Serialization & IL Emit | `cavekit-serialization.md` | 13 | 4 | ACTIVE | IL generation for serializers/deserializers, type mapping, custom type handling |
| 6 | SOAP Protocol & Runtime | `cavekit-protocol.md` | 13 | 6 | LEGACY | SOAP envelope construction, HTTP POST, response parsing, fault handling (superseded by cavekit-async-runtime) |
| 7 | Core Types & Contracts | `cavekit-core-types.md` | 12 | 4 | ACTIVE | Identifier types, XRoadHeader, custom attributes, choice helpers, endpoint config |
| 8 | Test Infrastructure & CI | `cavekit-test-infrastructure.md` | 14 | 4 | ACTIVE | Unit tests, integration tests, CI/CD workflows, multi-framework testing |
| 9 | HTTP Client Infrastructure | `cavekit-http-client-infrastructure.md` | 6 | 0 | NEW | Factory interface for HttpClient creation, secure TLS defaults, design-time ambient registration, certificate pinning |
| 10 | Design-Time HTTP Transport | `cavekit-design-time-http.md` | 9 | 0 | NEW | HttpClient-based WSDL/XSD fetching at compile-time, caching, meta-service calls, SSRF protection |
| 11 | Async Runtime API | `cavekit-async-runtime.md` | 13 | 0 | NEW | Async Task-based service call execution, cancellation, factory injection, resource disposal, SOAP faults |

**Totals:** 11 domains, 127 requirements, 37 identified gaps in legacy kits, 0 in new kits

## Cross-Reference Map

```
┌─────────────────────────────────────────────────────────┐
│                    Type Provider                         │
│              (cavekit-type-provider)                     │
│  • Static parameters, type discovery, caching           │
└──────────────────┬──────────────────────────────────────┘
                   │
        ┌──────────┴──────────┬──────────────┐
        ▼                     ▼              ▼
┌──────────────────┐ ┌──────────────┐ ┌──────────────────┐
│ HTTP Transport   │ │    Core      │ │   Schema         │
│ (R5, R6, R7)     │ │   Types      │ │   Parsing        │
│                  │ │ (R1, R2, R3) │ │ (R1, R2, R3)     │
└────────┬─────────┘ └──────────────┘ └────────┬─────────┘
         │                                     │
         └──────────────┬──────────────────────┘
                        ▼
               ┌──────────────────┐
               │  Type Generation │
               │  (R1, R2, R3)    │
               │ • ProvidedTypes  │
               │ • Custom attrs   │
               │ • Collections    │
               └────────┬─────────┘
                        │
        ┌───────────────┴───────────────┐
        ▼                               ▼
┌──────────────────┐         ┌──────────────────┐
│  Serialization   │         │  Protocol        │
│   & IL Emit      │         │  (Runtime SOAP)  │
│  • TypeMap       │         │ • Envelope       │
│  • IL delegates  │         │ • HTTP POST      │
└────────┬─────────┘         │ • Response       │
         │                   │ • Faults         │
         └───────────┬───────┘
                     │
                     ▼
            ┌──────────────────┐
            │ Core Types       │
            │ • Identifiers    │
            │ • Header         │
            │ • Attributes     │
            └──────────────────┘

Test Infrastructure (cavekit-test-infrastructure) spans all domains.
```

## Dependency Graph

**Build Order (layers, bottom-up) — Post-Migration Target State:**

1. **Foundation Layer** — No dependencies
   - cavekit-core-types (R1–R12: identifier types, attributes, header, AbstractEndpointDeclaration)
   - cavekit-test-infrastructure (R1–R14: testing framework)

2. **Infrastructure Layer** — Depends on Core
   - cavekit-http-client-infrastructure (R1–R6: factory interface, secure defaults, ambient registration, pinning)

3. **Data Layer** — Depends on Infrastructure + Core
   - cavekit-design-time-http (R1–R9: HttpClient-based WSDL/XSD fetch, caching, meta-services at compile-time)
   - cavekit-schema-parsing (R1–R11: WSDL/XSD parsing)
   - cavekit-serialization (R1–R13: IL-based serialization)
   - cavekit-async-runtime (R1–R13: Task<T> service call execution, cancellation, factory injection at runtime)

4. **Type Generation Layer** — Depends on Data + Core
   - cavekit-type-generation (R1–R11: schema → ProvidedTypes)

5. **Integration Layer** — Depends on all previous layers
   - cavekit-type-provider (R1–R12: type discovery, service enumeration)

**Critical Dependencies:**
- **HTTP Client Infrastructure** must complete first (both design-time and runtime depend on it)
- **Design-Time HTTP Transport** must complete before Schema Parsing (fetches WSDL at compile-time)
- **Schema Parsing** must complete before Type Generation (provides type definitions)
- **Type Generation** must complete before Type Provider (generates the types users see)
- **Serialization**, **Async Runtime API**, and **Type Generation** are developed in parallel (all use Core Types)
- **Async Runtime API** is injected into generated service port methods by Type Generation

**Legacy Kit Status:**
- `cavekit-http-transport` and `cavekit-protocol` are being superseded by `cavekit-http-client-infrastructure`, `cavekit-design-time-http`, and `cavekit-async-runtime`
- Legacy kits remain documented for reference during migration but should be phased out

## Gap Summary

**Critical Gaps** (security/safety):
- cavekit-http-transport: XXE protection [R9], TLS hardening [R8], certificate validation [R4]
- cavekit-protocol: Server certificate validation [R9], HTTP timeout [R10]
- cavekit-serialization: Recursion limit enforcement [R11], TypeMap thread safety [R10]

**Robustness Gaps** (error handling):
- cavekit-schema-parsing: Error accumulation instead of fail-fast [R8], recursion limits [R9]
- cavekit-type-generation: Keyword escaping [R8], thread safety [R10]
- cavekit-type-provider: Configuration validation [R11], error reporting [R8], cancellation support [R9]

**Quality Gaps** (test coverage):
- cavekit-test-infrastructure: Offline fixtures [R9], vulnerability scanning [R11], coverage metrics [R14]

**Usability Gaps**:
- cavekit-core-types: Parse error handling (exceptions vs. Result types) [R11]
- cavekit-http-transport: Request timeouts [R11], SSRF prevention [R12], multipart size limits [R13]
- cavekit-protocol: Response streaming [R12], request logging [R13]
- cavekit-serialization: Error context [R12], custom hooks [R13]

**Total Gap Count by Severity:**
- **HIGH (security):** 4 gaps (XXE, TLS, cert validation, timeout)
- **MEDIUM (robustness):** 12 gaps (error handling, thread safety, validation)
- **LOW (quality):** 21 gaps (testing, logging, optimization)

## Key Design Decisions

### 1. Split-Assembly Type Provider
- **Design-time assembly** (`FSharp.Data.XRoad.DesignTime`): generates types at compile time
- **Runtime assembly** (`FSharp.Data.XRoad`): ships with compiled code, contains types for consumers
- **Benefit:** Type provider SDK requires no external runtime dependencies; types are baked into user code
- **Tradeoff:** Code duplication (Attributes, Choices, MetaServices compiled into both)

### 2. IL-Based Serialization
- Custom IL generation at runtime for serialization/deserialization delegates
- **Benefit:** Type-safe, performant (compiled IL, not reflection)
- **Tradeoff:** Complex code, large IL emit surface (1862 lines), hard to test in isolation

### 3. NodaTime for Dates
- All date/time types use NodaTime (OffsetDate, OffsetDateTime, OffsetTime, Period) instead of BCL
- **Benefit:** ISO 8601 semantics, timezone-aware, matches XSD exactly
- **Tradeoff:** New dependency, users must understand NodaTime

### 4. X-Road v6 Only
- Only X-Road v6 protocol is supported (with protocol version "4.0" in SOAP headers)
- **Benefit:** Simpler codebase, focused implementation
- **Tradeoff:** No support for legacy X-Road versions

### 5. SOAP 1.1 Document/Literal
- WSDL operations are SOAP 1.1, document/literal binding only
- **Benefit:** Most common and interoperable format
- **Tradeoff:** No SOAP 1.2, no RPC-style encoding

## Risk & Mitigation Summary

| Risk | Severity | Current Status | Mitigation |
|------|----------|----------------|-----------|
| **XXE Injection** | CRITICAL | [GAP in cavekit-http-transport] XXE not hardened | XXE protection in cavekit-design-time-http R1, R4 |
| **HTTPS Cert Validation Disabled** | CRITICAL | [GAP in cavekit-protocol] Validation always passes | Cert validation in cavekit-http-client-infrastructure R2; pinning in R5 |
| **Server Certificate Pinning** | HIGH | Not implemented | Implemented in cavekit-http-client-infrastructure R5 |
| **HTTP Timeout Hang** | HIGH | [GAP in cavekit-protocol] No timeout set | Timeout in cavekit-http-client-infrastructure R2; configurable in R6 |
| **Resource Leaks** | HIGH | [GAP in cavekit-protocol] WebResponse not disposed | Resource disposal in cavekit-design-time-http R4, cavekit-async-runtime R4 |
| **Thread Safety in IL Emit** | HIGH | [GAP in cavekit-serialization] TypeMap.IsComplete mutable | Addressed separately in cavekit-serialization |
| **Stack Overflow on Recursive Schema** | HIGH | [GAP in cavekit-schema-parsing] No depth limit | Addressed separately in cavekit-schema-parsing |
| **Memory Exhaustion (Multipart)** | MEDIUM | [GAP in cavekit-http-transport] Unbounded multipart reads | Size limits in cavekit-design-time-http R9, cavekit-async-runtime R12 |
| **Error Feedback (fail-fast)** | MEDIUM | [GAP in cavekit-schema-parsing] Errors stop immediately | Addressed separately in cavekit-schema-parsing |
| **Synchronous API Blocking** | HIGH | Current implementation blocks type provider execution | Async-first design in cavekit-async-runtime; design-time blocking at boundary in cavekit-design-time-http R6 |
| **Global ServicePointManager Mutation** | HIGH | [GAP in cavekit-http-transport] Global side effects | Eliminated: cavekit-http-client-infrastructure uses isolated HttpClientHandler per factory instance |
| **No Injection Point for Testing** | MEDIUM | Monolithic protocol code | Addressed: cavekit-http-client-infrastructure provides injectable factory; cavekit-async-runtime R3 injects per endpoint |

## Testing & Validation Checklist

**Critical Path Tests** (must pass before release):
- [ ] Primitive type serialization/deserialization (int, string, bool, decimal, datetime)
- [ ] Complex type with nested properties
- [ ] Choice type (union) with correct case selection
- [ ] Collection (list) of items
- [ ] Nullable property (xsi:nil)
- [ ] Service call end-to-end (mocked endpoint)
- [ ] Type provider generates correct types for sample WSDL
- [ ] Multi-framework build (net472, net10.0)
- [ ] XXE rejection test
- [ ] Invalid date parse error (clear message)

**Regression Tests** (per gap):
- [ ] Certificate validation is enforced (not bypassed)
- [ ] HTTP timeout is enforced
- [ ] WebResponse is disposed
- [ ] Thread-safe type registration (concurrent calls)
- [ ] Large multipart response is rejected (size limit)
- [ ] Deep schema (100+ levels) is rejected (recursion limit)

## Roadmap Recommendations

### Phase 1 (Security Hardening) — HIGH PRIORITY
1. **cavekit-http-transport**: Implement XXE protection [R9], TLS hardening [R8]
2. **cavekit-protocol**: Implement server certificate validation [R9], HTTP timeout [R10]
3. **cavekit-serialization**: Add TypeMap thread safety [R10]
4. **cavekit-test-infrastructure**: Add XXE and timeout tests [R13]

### Phase 2 (Robustness) — MEDIUM PRIORITY
1. **cavekit-schema-parsing**: Error accumulation [R8], recursion limits [R9]
2. **cavekit-type-provider**: Configuration validation [R11], error reporting [R8]
3. **cavekit-http-transport**: Multipart size limits [R13], SSRF prevention [R12]
4. **cavekit-test-infrastructure**: Offline fixtures [R9], negative tests [R13]

### Phase 3 (Quality & Testing) — LOWER PRIORITY
1. **cavekit-test-infrastructure**: Coverage metrics [R14], vulnerability scanning [R11]
2. **cavekit-protocol**: Response streaming [R12], request logging [R13]
3. **cavekit-core-types**: Result-based parse errors [R11]
4. Documentation and API stability

## How to Use These Cavekits

1. **For Implementation Planning**: Use the Dependency Graph to determine build order and parallelization opportunities.
2. **For Code Review**: Reference relevant acceptance criteria to validate that PRs meet requirements.
3. **For Testing**: Acceptance criteria define what automated tests should verify.
4. **For Refactoring**: Use Out of Scope sections to avoid scope creep; Gaps show where robustness improvements are needed.
5. **For Risk Management**: Risk summary shows critical areas requiring extra review.

Each cavekit is self-contained and can be read independently. Cross-References section in each kit links to related domains.

---

## HttpClient Migration Summary

**Motivation:** Replace legacy `HttpWebRequest`/`ServicePointManager` patterns with modern `HttpClient` for security, maintainability, and async support.

**Breaking Change:** Runtime service call API transitions from synchronous to async (Task<T>). Existing code must be updated to use async/await.

**New Design Principles:**
1. **Injection over Globals:** HttpClient is obtained from injectable factory, not global state
2. **Async-First Runtime:** All service calls are Task<T>; optional [Obsolete] sync wrapper for migration
3. **Secure by Default:** TLS 1.2/1.3 only, certificate validation enforced, pinning available
4. **Design-Time Synchronization:** Type provider blocks on async HTTP (maintains compile-time sync semantics)
5. **Resource Management:** All streams/responses properly disposed; no ServicePointManager mutations

**Implementation Timeline:**
- Phase 1: Build cavekit-http-client-infrastructure + update core-types for factory injection
- Phase 2: Implement cavekit-design-time-http (type provider HTTP operations)
- Phase 3: Implement cavekit-async-runtime (service call execution)
- Phase 4: Update type generation to emit async methods
- Phase 5: Testing, migration guide, documentation

---

**Document Version:** 1.1  
**Last Updated:** 2026-04-17  
**Domains:** 11 | **Requirements:** 127 | **New Kits:** 3 | **Migration Target State**
