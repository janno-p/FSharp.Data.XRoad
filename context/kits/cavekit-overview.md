---
created: "2026-04-16T00:00:00Z"
last_edited: "2026-04-16T00:00:00Z"
---

# Cavekit Overview: FSharp.Data.XRoad

## Project Summary

FSharp.Data.XRoad is an F# type provider for the X-Road (Nordic/Baltic e-government) data exchange layer. It enables F# developers to consume X-Road services with full type safety at compile time. The system consists of two assemblies: a design-time type provider assembly (generates types from WSDL) and a runtime assembly (executes SOAP requests and handles serialization).

This document indexes all domain Cavekits and their relationships.

## Domain Index

| # | Domain | File | Req Count | Gaps | Status | Description |
|---|--------|------|-----------|------|--------|-------------|
| 1 | HTTP Transport & Security | `cavekit-http-transport.md` | 13 | 7 | ACTIVE | HTTPS/HTTP document fetching, caching, certificate validation, multipart MIME parsing |
| 2 | WSDL & Schema Parsing | `cavekit-schema-parsing.md` | 11 | 3 | ACTIVE | WSDL document model, XSD type parsing, namespace handling, composition |
| 3 | Type Generation | `cavekit-type-generation.md` | 11 | 3 | ACTIVE | ProvidedTypeDefinition generation, inheritance, custom attributes, collections |
| 4 | Type Provider Infrastructure | `cavekit-type-provider.md` | 12 | 6 | ACTIVE | Type provider entry points, service discovery, static parameters, caching |
| 5 | Serialization & IL Emit | `cavekit-serialization.md` | 13 | 4 | ACTIVE | IL generation for serializers/deserializers, type mapping, custom type handling |
| 6 | SOAP Protocol & Runtime | `cavekit-protocol.md` | 13 | 6 | ACTIVE | SOAP envelope construction, HTTP POST, response parsing, fault handling |
| 7 | Core Types & Contracts | `cavekit-core-types.md` | 12 | 4 | ACTIVE | Identifier types, XRoadHeader, custom attributes, choice helpers, endpoint config |
| 8 | Test Infrastructure & CI | `cavekit-test-infrastructure.md` | 14 | 4 | ACTIVE | Unit tests, integration tests, CI/CD workflows, multi-framework testing |

**Totals:** 8 domains, 99 requirements, 37 identified gaps

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

**Build Order (layers, bottom-up):**

1. **Foundation Layer** — No dependencies
   - cavekit-core-types (R1–R12: identifier types, attributes, header)
   - cavekit-test-infrastructure (R1–R14: testing framework)

2. **Data Layer** — Depends on Core
   - cavekit-http-transport (R1–R13: HTTP fetching, caching, multipart)
   - cavekit-schema-parsing (R1–R11: WSDL/XSD parsing)
   - cavekit-serialization (R1–R13: IL-based serialization)

3. **Type Generation Layer** — Depends on Data + Core
   - cavekit-type-generation (R1–R11: schema → ProvidedTypes)

4. **Integration Layer** — Depends on all previous layers
   - cavekit-type-provider (R1–R12: type discovery, service enumeration)
   - cavekit-protocol (R1–R13: SOAP request/response handling)

**Critical Dependencies:**
- **HTTP Transport** must complete before Schema Parsing (fetches WSDL)
- **Schema Parsing** must complete before Type Generation (provides type definitions)
- **Type Generation** must complete before Type Provider (generates the types users see)
- **Serialization** and **Protocol** are developed in parallel (both use Core Types)

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
| **XXE Injection** | CRITICAL | [GAP] XXE not hardened | Add XmlReaderSettings with DTD disabled |
| **HTTPS Cert Validation Disabled** | CRITICAL | [GAP] Validation always passes | Implement proper certificate chain validation, add pinning support |
| **Server Certificate Pinning** | HIGH | Not implemented | Add thumbprint-based pinning option |
| **HTTP Timeout Hang** | HIGH | [GAP] No timeout set | Set request.Timeout before GetResponse() |
| **Resource Leaks** | HIGH | [GAP] WebResponse not disposed | Wrap response in using statement |
| **Thread Safety in IL Emit** | HIGH | [GAP] TypeMap.IsComplete mutable | Use lock() or Interlocked for thread-safe registration |
| **Stack Overflow on Recursive Schema** | HIGH | [GAP] No depth limit | Track recursion depth, fail at limit |
| **Memory Exhaustion (Multipart)** | MEDIUM | [GAP] Unbounded multipart reads | Add size limits, enforce per-attachment max |
| **Error Feedback (fail-fast)** | MEDIUM | [GAP] Errors stop immediately | Accumulate errors, report all at end |
| **Performance Regression** | MEDIUM | No baseline | Establish performance baselines in CI |

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

**Document Version:** 1.0  
**Last Updated:** 2026-04-16  
**Domains:** 8 | **Requirements:** 99 | **Gaps:** 37 | **Critical Issues:** 4
