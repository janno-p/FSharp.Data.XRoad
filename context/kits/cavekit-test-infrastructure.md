---
created: "2026-04-16T00:00:00Z"
last_edited: "2026-04-16T00:00:00Z"
---

# Cavekit: Test Infrastructure & CI

## Scope

This cavekit covers test suites (unit, integration, E2E) and continuous integration pipelines. It includes:
- xUnit test projects for runtime behavior (serialization, deserialization, protocol)
- Design-time tests for type provider functionality
- Test fixtures and helpers
- CI/CD workflows (GitHub Actions)
- NuGet package publishing pipeline
- Multi-framework test targets (net472, net10.0, netstandard2.0, netstandard2.1)
- Optional live X-Road endpoint testing

This cavekit does NOT include the test implementations themselves (those are separate from requirements), but describes the testing infrastructure and coverage expectations.

## Requirements

### R1: Serialization Unit Tests
**Description:** Runtime serialization (primitives, complex types, dates) must be tested comprehensively.

**Acceptance Criteria:**
- [ ] Primitive types (string, int, boolean, decimal, float) serialize and deserialize correctly
- [ ] NodaTime types (OffsetDate, OffsetDateTime, OffsetTime, Period) round-trip correctly with timezone info
- [ ] Complex types (objects with properties) serialize and deserialize with correct nesting
- [ ] Optional properties (nullable, minOccurs=0) serialize as xsi:nil when absent
- [ ] Collections (lists, arrays) serialize as repeated elements
- [ ] Choice types serialize with correct union case selection
- [ ] Nested complex types work correctly
- [ ] Test coverage is >= 80% for serialization code

**Dependencies:** cavekit-serialization

### R2: Deserialization Error Handling Tests
**Description:** Deserialization must fail gracefully with clear errors for invalid input.

**Acceptance Criteria:**
- [ ] Invalid date/time strings fail with readable parse error
- [ ] Missing required elements fail with element name and type
- [ ] Type mismatch (string when int expected) fails with clear error
- [ ] Extra unknown elements are skipped (forward compatibility)
- [ ] Malformed XML fails with XmlException or similar
- [ ] Nil element with non-nullable property fails appropriately

**Dependencies:** cavekit-serialization

### R3: Complex Type Tests
**Description:** Complex types with various compositions must work correctly.

**Acceptance Criteria:**
- [ ] Sequence-based types (most common) work
- [ ] Choice-based types work with correct case selection
- [ ] Types with inheritance (extension) work
- [ ] Abstract types are marked and handled
- [ ] Nested types work
- [ ] Type with many properties (>10) works

**Dependencies:** cavekit-serialization, cavekit-type-generation

### R4: SOAP Multipart and Attachments Tests
**Description:** MTOM and SwA (Soap with Attachments) must be tested.

**Acceptance Criteria:**
- [ ] Multipart response parsing works
- [ ] Binary attachments are extracted correctly
- [ ] MTOM xop:Include references are resolved
- [ ] Single-part responses work without error
- [ ] Attachment Content-ID is preserved

**Dependencies:** cavekit-http-transport (R3), cavekit-protocol (R5)

### R5: Service Operation Tests
**Description:** End-to-end service calls must work (with mocked or real endpoints).

**Acceptance Criteria:**
- [ ] Service method can be invoked with parameters
- [ ] Request is serialized correctly
- [ ] Response is deserialized correctly
- [ ] Result is returned to caller
- [ ] Exceptions are propagated correctly

**Dependencies:** All domains

### R6: Design-Time Type Provider Tests
**Description:** Type provider must generate correct types from WSDL.

**Acceptance Criteria:**
- [ ] Type provider accepts static parameters
- [ ] WSDL is fetched and parsed
- [ ] Types are generated for each service/operation
- [ ] Generated types compile and work at runtime
- [ ] Type names are correct
- [ ] Custom attributes are applied

**Dependencies:** cavekit-type-provider, cavekit-type-generation

### R7: Multi-Framework Testing
**Description:** Tests must run on all supported .NET versions.

**Acceptance Criteria:**
- [ ] Tests run on .NET Framework 4.7.2 (net472)
- [ ] Tests run on .NET 10.0 (net10.0) — current LTS/STS
- [ ] Tests run on netstandard2.0 and netstandard2.1 (if applicable)
- [ ] All test projects target multiple frameworks
- [ ] CI runs tests on all frameworks

**Dependencies:** None (infrastructure)

### R8: Live Endpoint Testing (Optional)
**Description:** Tests can optionally connect to a real X-Road security server for integration testing.

**Acceptance Criteria:**
- [ ] Appsettings.user.json can configure live endpoint
- [ ] When configured, tests connect to real security server
- [ ] Tests enumerate real producers and services
- [ ] Tests call real service methods and verify responses
- [ ] When not configured, tests skip or use mocks
- [ ] Live tests are marked as [Explicit] or conditional (skip by default in CI)

**Dependencies:** cavekit-http-transport, cavekit-type-provider

### R9: Offline Test Fixtures [GAP]
**Description:** Tests should not depend on live endpoints; offline fixtures should be provided.

**Acceptance Criteria:**
- [ ] Sample WSDL files are provided in tests/ directory
- [ ] Sample XSD schemas are provided
- [ ] Sample SOAP responses are canned (pre-recorded)
- [ ] Mock HTTP server or mocking library is used (e.g., Moq, NSubstitute)
- [ ] Schema parsing tests use offline fixtures
- [ ] Builder/type generation tests use offline fixtures
- [ ] Serialization tests use offline fixtures

**Dependencies:** None (test infrastructure)

**Status:** [GAP] Current code: Design-time tests depend on live endpoints (lines in DesignTime.Tests/appsettings.json). No fixtures provided. Schema parsing has no offline test coverage.

### R10: CI/CD Pipeline
**Description:** GitHub Actions workflows must build, test, and publish the project.

**Acceptance Criteria:**
- [ ] Build workflow runs on every push/PR to main
- [ ] Tests are run on multiple OS (Ubuntu, Windows)
- [ ] Tests run on all supported frameworks
- [ ] Build fails if tests fail
- [ ] NuGet package is created with correct version
- [ ] Package publishing only occurs on tagged releases
- [ ] Build artifacts are available for inspection

**Dependencies:** None (infrastructure)

### R11: Dependency Vulnerability Scanning [GAP]
**Description:** Dependencies should be scanned for known security vulnerabilities.

**Acceptance Criteria:**
- [ ] `dotnet list package --vulnerable` is run in CI
- [ ] Vulnerable dependencies are reported and updated
- [ ] CI fails if vulnerable dependencies exist (with known CVEs)
- [ ] Paket.lock is kept up-to-date

**Dependencies:** None (security)

**Status:** [GAP] Current code: No vulnerability scanning in CI/CD. No `dotnet list package` check.

### R12: Package Versioning [GAP]
**Description:** NuGet package version must be derived from git tags or version files.

**Acceptance Criteria:**
- [ ] Package version is not hardcoded in CI workflow
- [ ] Version is derived from git tag (e.g., v1.2.3 → 1.2.3)
- [ ] Version is derived from version.txt or .csproj property if no tag
- [ ] Pre-release versions (beta, alpha) are supported
- [ ] Version increment is deterministic and follows semver

**Dependencies:** None (versioning)

**Status:** [GAP] Current code: NuGet pack version hardcoded as "1.0.0-beta000" in CI workflow (line in publish.yml).

### R13: Negative Tests [GAP]
**Description:** Tests for error cases and malformed inputs should be comprehensive.

**Acceptance Criteria:**
- [ ] Malformed WSDL is tested (missing elements, invalid structure)
- [ ] XXE payloads are tested (should be rejected)
- [ ] Circular schema references are tested
- [ ] Very deeply nested schemas are tested (recursion limit)
- [ ] Invalid HTTP responses are tested
- [ ] Timeout scenarios are tested
- [ ] Certificate validation errors are tested

**Dependencies:** All domains

**Status:** [GAP] Current code: Minimal negative test coverage. No XXE, circular ref, or timeout tests.

### R14: Test Coverage Metrics [GAP]
**Description:** Code coverage should be measured and reported.

**Acceptance Criteria:**
- [ ] Code coverage >= 80% for production code
- [ ] Coverage report is generated in CI
- [ ] Coverage report is available for inspection
- [ ] Coverage badge is displayed in README (optional)

**Dependencies:** None (measurement)

**Status:** [GAP] Current code: No coverage measurement in CI. No coverage tool configured.

## Out of Scope

- Performance benchmarking (optional, not core testing)
- Manual testing procedures or test checklists (those are process documentation)
- Visual regression testing (not applicable; library code)
- Load testing for X-Road infrastructure (out of scope; that's X-Road testing, not library testing)
- Security penetration testing (covered by negative tests and security review)

## Cross-References

- **All domains**: Each domain has its own test requirements
- **cavekit-http-transport**: Tests for HTTP fetching, certificate validation, multipart parsing
- **cavekit-schema-parsing**: Tests for WSDL and XSD parsing
- **cavekit-type-generation**: Tests for type generation from schema
- **cavekit-serialization**: Tests for IL-based serialization and deserialization
- **cavekit-protocol**: Tests for SOAP envelope construction and response handling
- **cavekit-type-provider**: Tests for type provider discovery and type generation

## Source Traceability

**Source files (tests):**
- `tests/FSharp.Data.XRoad.Tests/` (10 test files, ~1400 lines)
  - FSharp.Data.XRoad.Serialization.Date.fs — Date serialization tests
  - FSharp.Data.XRoad.Serialization.DateTime.fs — DateTime tests
  - FSharp.Data.XRoad.Serialization.Time.fs — Time tests
  - FSharp.Data.XRoad.Serialization.SwaRef.fs — Attachment tests
  - FSharp.Data.XRoad.Serialization.AnyType.fs — anyType handling
  - FSharp.Data.XRoad.SimpleTypes.fs — Primitive type tests
  - FSharp.Data.XRoad.ComplexTypes.fs — Complex type tests
  - FSharp.Data.XRoad.Elements.fs — Element and collection tests
  - FSharp.Data.XRoad.Services.fs — Service invocation tests
  - FSharp.Data.XRoad.XRoadServerTests.fs — Live endpoint tests (optional)
  - FSharp.Data.XRoad.Helpers.fs — Test utilities

- `tests/FSharp.Data.XRoad.DesignTime.Tests/` (2 files, ~177 lines)
  - Common.fs — Test setup and helpers
  - Library.fs — Type provider tests

**Source files (CI/CD):**
- `.github/workflows/general.yml` — Build and test workflow
- `.github/workflows/publish.yml` — NuGet package publish workflow

**Project files:**
- `tests/FSharp.Data.XRoad.Tests/FSharp.Data.XRoad.Tests.fsproj` — Runtime tests
- `tests/FSharp.Data.XRoad.DesignTime.Tests/FSharp.Data.XRoad.DesignTime.Tests.fsproj` — Design-time tests

**Configuration:**
- `tests/FSharp.Data.XRoad.DesignTime.Tests/appsettings.json` — Default (mock) settings
- `tests/FSharp.Data.XRoad.DesignTime.Tests/appsettings.user.json` — Live endpoint config (optional)

**Typical test flow:**
1. Developer runs `dotnet test` locally (all frameworks, all platforms)
2. CI runs `dotnet build` and `dotnet test` on every commit
3. On tagged release, CI runs `paket pack` to create NuGet package
4. NuGet package is published to nuget.org
5. Coverage report is generated and reviewed
6. Security scan is performed on dependencies
