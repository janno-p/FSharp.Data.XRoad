# FSharp.Data.XRoad — Codebase Review

**Date:** 2026-04-16  
**Reviewer:** Claude Code (parallel agent analysis: security-reviewer, code-reviewer, architect, test/CI specialist)  
**Scope:** Full repository at `/mnt/c/Work/FSharp.Data.XRoad/`  
**Verdict:** **BLOCK release** — one CRITICAL security issue (TLS bypass) plus multiple HIGH issues require remediation before the next NuGet publish.

---

## 1. Executive Summary

| Area | Grade | Key Issues |
|------|-------|------------|
| Security | D | TLS cert validation bypass (CRITICAL); XXE-prone XML loading; unbounded multipart reads |
| Code Quality | C | 3 files over 1000 lines; runtime/design-time source duplication; sparse error handling |
| Architecture | C+ | Solid split-assembly pattern; poor cohesion in Emit/Builder/Schema; no protocol version abstraction |
| Tests | D | No unit tests for IL emit, Schema parser, Builder; design-time tests require live security server |
| CI/CD | C- | Framework matrix gap between Ubuntu and Windows; hardcoded version; no dependency audit |
| Dependencies | C | NodaTime, Optional, System.Reflection.Emit.* unpinned in `paket.dependencies` |
| Documentation | F | 17-line README; no CONTRIBUTING/CHANGELOG/SECURITY docs; no API samples |

Overall quality score: **~35/100** relative to production-grade OSS libraries.

---

## 2. CRITICAL Issues (block release)

### C1. TLS certificate validation disabled for HTTPS — `Http.fs:20`
```fsharp
if uri.Scheme = "https" then request.ServerCertificateValidationCallback <- (fun _ _ _ _ -> true)
```
Any MITM attacker on the network path can serve a forged WSDL/XSD. Because the type provider consumes remotely fetched XML to generate code that a downstream application will execute, this is effectively a compile-time code-injection vector.

**Fix:** Remove the callback entirely. If certificate pinning is needed (valid for X-Road deployments that use self-signed server certs), validate the thumbprint against a configured value and fail-closed otherwise:
```fsharp
// remove the blanket override; optionally pin by thumbprint
request.ServerCertificateValidationCallback <-
    (fun _ cert _ errors ->
        match errors, expectedThumbprint with
        | SslPolicyErrors.None, _ -> true
        | _, Some thumb when (cert :?> X509Certificate2).Thumbprint = thumb -> true
        | _ -> false)
```

### C2. XML External Entity (XXE) risk in WSDL/XSD loading — `Http.fs:45,58,193-194`
```fsharp
XDocument.Load(contentStream)
XDocument.Load(file.OpenRead())
XDocument.Load(responseStream)
XDocument.Load(uri.ToString())
```
`XDocument.Load` on .NET Framework 4.7.2 uses settings where DTD processing is enabled by default. A malicious WSDL can exfiltrate files via external entities, trigger billion-laughs DoS, or pivot through internal services via SYSTEM entities. Equivalent exposure exists in the runtime SOAP response path (`Protocol.fs:30,57` — `XmlReader.Create(stream)` without explicit settings).

**Fix:** Centralize a hardened `XmlReaderSettings` and use it everywhere:
```fsharp
let private safeXmlSettings () =
    XmlReaderSettings(
        DtdProcessing = DtdProcessing.Prohibit,
        XmlResolver = null,
        MaxCharactersFromEntities = 1_000_000L,
        MaxCharactersInDocument = 50_000_000L)

let loadSafe (stream: Stream) =
    use reader = XmlReader.Create(stream, safeXmlSettings ())
    XDocument.Load(reader)
```

---

## 3. HIGH Issues (fix before next release)

### H1. Unbounded multipart/MIME body reads — `Http.fs:246-353` (MultipartMessage)
The multipart parser grows attachment buffers indefinitely (`Array.Resize` in a loop over `CHUNK_SIZE = 4096`). A hostile or compromised security server can OOM the caller by streaming large parts.

**Fix:** Enforce a configurable max size per part and a total-response cap; abort the read when exceeded.

### H2. Resource leak — unclosed stream in `Http.getFile` — `Http.fs:58`
```fsharp
XDocument.Load(file.OpenRead())  // stream never disposed
```
On Windows this keeps temp-file handles open per cache entry and can block later deletion.

**Fix:** `use stream = file.OpenRead()` then `XDocument.Load(stream)`.

### H3. SSRF / unvalidated schema imports — `Schema.fs:~899-905`, `Http.fs:188-204`
`resolveUri` falls back to `FileInfo(uri).FullName` when the string is not a well-formed absolute URI, and `fixUri` resolves arbitrary relative paths against WSDL base. A malicious WSDL can reference `file:///etc/passwd`, `http://169.254.169.254/...` (cloud metadata), or internal services. No scheme allowlist, no domain policy.

**Fix:** Restrict schemes to `http`/`https` (and `file` only for explicitly-local roots), require imported schemas to share the same origin or an allowlist provided by the consumer, and document the policy in README.

### H4. TLS 1.0 / 1.1 enabled globally — `Http.fs:17`
```fsharp
ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls
```
Enabling deprecated TLS versions is a downgrade risk. Also: mutating `ServicePointManager` is a global side-effect that bleeds into the hosting process.

**Fix:** Set to `Tls12 ||| Tls13`, or better, avoid touching `ServicePointManager` and let the runtime default win.

### H5. Monolithic files violate project convention (>800 lines)
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Emit.fs` — **1862** lines
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Builder.fs` — **1706** lines
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Schema.fs` — **1013** lines
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.fs` — 543 lines

**Suggested splits:**
- `Emit.fs` → `EmitHelpers`, `EmitPrimitives`, `EmitCollections`, `EmitComplex`
- `Builder.fs` → `TypeGeneration`, `QuotationHelpers`, `ValidationCollector`, `ChoiceBuilder`
- `Schema.fs` → `SchemaDomain` (types only), `SchemaParser`, `SchemaValidator`

### H6. Design-time tests depend on live X-Road instances — `tests/FSharp.Data.XRoad.DesignTime.Tests/Library.fs:24-59`
Only six smoke tests exist, all hitting `rr`, `estat`, `ehis`, `kutseregister`, `kma`, `kp saatmine`. They will fail deterministically in CI if the network or the services are unavailable, masking regressions behind transient infra noise. No offline tests cover Schema, Builder, IL-emit.

**Fix:** Add a `DesignTime.UnitTests` project that consumes canned WSDL/XSD fixtures (check a small set into `tests/fixtures/`). Keep the live-server suite gated behind an env var.

### H7. CI framework matrix inconsistency — `.github/workflows/general.yml:28 vs :46`
Ubuntu job runs only `net10.0`; Windows runs both `net472` and `net10.0`. `net472`-specific issues (different `XmlReaderSettings` defaults, distinct `System.Net` stack) go untested on Linux.

**Fix:** Run both TFMs on both OSes, or drop `net472` entirely (see M4).

---

## 4. MEDIUM Issues

### M1. `XRoadResponse` is not reentrant-safe and leaks the underlying `WebResponse` — `Protocol.fs:24-81`
Constructor calls `request.GetResponse()` eagerly; `IDisposable` only disposes the captured `MemoryStream`, not the `response` itself. If `RetrieveMessage` throws before the response is consumed, the socket/connection leaks.

**Fix:** Dispose `response` in `Dispose`, and wrap the body with try/finally.

### M2. Hardcoded constants without names
- `Protocol.fs:101-105` — `1000` byte buffer, duplicated
- `Http.fs` — `CHUNK_SIZE = 4096` only in MultipartMessage

Extract literals to well-named constants in one place.

### M3. `SerializerContext` mutable attachment map
`AddAttachment` mutates shared state. If a consumer reuses one context across requests (they can, since nothing forbids it), interleaved requests can corrupt each other.

**Fix:** Document scope-of-use or switch to an immutable `with Attachments = ...` pattern.

### M4. Legacy target frameworks
`netstandard2.0` (2016) and `net472` (2019) add matrix cost. .NET 6+ is GA since 2021; .NET 10 is current. Dropping `net472` removes IL-emit polyfill packages and cuts CI time ~2×.

**Fix:** Target `netstandard2.1` + `net8.0` (or `net10.0`) only. Announce the drop in CHANGELOG.

### M5. `FSharp.Core` pinned to `6.0.7`
Current is 8.x; several ergonomics and performance improvements missed. Upgrade is low-risk if consumers are on recent SDKs.

### M6. `paket.dependencies` leaves several packages unpinned
```
nuget NodaTime
nuget Optional
nuget System.Reflection.Emit.ILGeneration
nuget System.Reflection.Emit.Lightweight
```
Silent version drift on restore. Pin or use lower/upper bounds.

### M7. Swallowed/opaque errors in Schema parser
Many `failwith $"..."` sites (Schema.fs, Builder.fs). Messages are useful, but they abort the whole pipeline at first error and surface as F# compile-time errors without line/column context. This produces a frustrating developer experience on malformed WSDL.

**Fix:** Collect errors via `Result`, surface as provider diagnostics; keep a "strict mode" toggle.

### M8. No dependency-vulnerability audit in CI
Add `dotnet list package --vulnerable --include-transitive` gate to `general.yml`.

### M9. Hardcoded NuGet package version in both workflow jobs
`general.yml:32,50` and `publish.yml` use `1.0.0-beta000`. Release numbering should derive from the tag or `${{ github.run_number }}`.

### M10. `appsettings.user.json` override mechanism is fragile
Tests read `appsettings.json` then optionally override with `appsettings.user.json`. A committed empty `appsettings.json` is fine, but there is no validation that local overrides don't ship in the published package; confirm `paket.template` excludes test artifacts.

---

## 5. LOW Issues / Hygiene

- L1. `FSharp.Data.XRoad.Runtime.fs` is a 5-line file that only holds assembly attributes — either fold into `FSharp.Data.XRoad.fs` or rename to `AssemblyInfo.fs`.
- L2. TODO in `Protocol.fs:66` has no issue tracker reference.
- L3. `README.md` is 17 lines — lacks feature overview, usage sample, supported frameworks, X-Road protocol versions, known limitations.
- L4. No `CHANGELOG.md`, `CONTRIBUTING.md`, `SECURITY.md` (vulnerability disclosure policy).
- L5. XPath literal in `Protocol.fs:17` is not user-controlled, so safe today. Keep a comment to warn against future interpolation.
- L6. `StreamWriter` in `Protocol.fs:111` is not explicitly disposed/flushed; rely on GC is brittle for text writers.
- L7. Copyright range in `LICENSE.md` is 2019–2026 — confirm current year is accurate for next release.

---

## 6. Architectural Observations

**Strengths**
- Split-assembly pattern is correct and matches FSharp.TypeProviders.SDK conventions.
- Functional pipeline (Xml → Wsdl → Schema → Builder → DesignTime) has clear single responsibilities at module level.
- NodaTime for XSD date/time semantics is the right call.
- Identifier types (`XRoadMemberIdentifier`, `XRoadServiceIdentifier`) are immutable value objects.

**Weaknesses**
- No abstraction over X-Road protocol versions (v6 hardcoded). Adding v7 or a regional variant will ripple through Protocol/MetaServices/Schema.
- Shared runtime source files are recompiled into both assemblies rather than referenced. Consider a third `FSharp.Data.XRoad.Shared` assembly targeting `netstandard2.1`, containing Protocol + MultipartMessage + SerializerContext.
- IL-emit is the right tool for serialization hot paths today, but a future migration to source generators (`IIncrementalGenerator` / F# type-provider SDK equivalents) would drastically simplify debugging.

---

## 7. Prioritized Remediation Roadmap

### Sprint 1 — Block-release fixes (2–3 days)
1. **C1** remove TLS callback override (`Http.fs:20`)
2. **C2** centralize safe `XmlReaderSettings`; replace every `XDocument.Load` / `XmlReader.Create` call
3. **H4** restrict to TLS 1.2/1.3
4. **H1** bound multipart reads
5. **H2** fix stream leak in `getFile`
6. **M9** parametrize NuGet pack version from tag/run number
7. Add a regression test that loads a WSDL with an external entity and asserts it's rejected

### Sprint 2 — Quality gates (1 week)
8. **H7** harmonize CI matrix (both OS × both TFMs)
9. **M6** pin all dependencies
10. **M8** add `dotnet list package --vulnerable` gate
11. **H6** add offline DesignTime unit tests with canned WSDL fixtures
12. **H3** implement URI allowlist for schema imports
13. **M1** fix `XRoadResponse` disposal

### Sprint 3 — Refactor for maintainability (2 weeks)
14. **H5** split `Emit.fs`, `Builder.fs`, `Schema.fs`
15. **M4** drop `net472`/`netstandard2.0`, bump to `netstandard2.1` + `net8.0`
16. **M5** upgrade `FSharp.Core` to 8.x
17. **M7** convert Schema/Builder error flow to `Result`-based diagnostics
18. Extract `FSharp.Data.XRoad.Shared` assembly

### Sprint 4 — Product polish
19. Expand README with usage examples and limitations
20. Add CHANGELOG.md, CONTRIBUTING.md, SECURITY.md
21. Introduce `IXRoadProtocol` abstraction to prepare for future protocol versions
22. Enable code coverage in CI (Coverlet + Codecov)

---

## 8. Appendix — Verified File References

| Finding | File | Line |
|---------|------|------|
| C1 TLS bypass | `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Http.fs` | 20 |
| C2 XXE (design-time) | `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Http.fs` | 45, 58, 193-194 |
| C2 XXE (runtime) | `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Protocol.fs` | 30, 57 |
| H1 Unbounded multipart | `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Http.fs` | 207–353 |
| H2 Stream leak | `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Http.fs` | 58 |
| H3 SSRF | `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Http.fs` | 188–204; Schema.fs (fixUri) |
| H4 TLS downgrade | `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Http.fs` | 17 |
| H5 Giant files | `Emit.fs` 1862, `Builder.fs` 1706, `Schema.fs` 1013 | — |
| H6 No offline DT tests | `tests/FSharp.Data.XRoad.DesignTime.Tests/Library.fs` | 24–59 |
| H7 CI gap | `.github/workflows/general.yml` | 28 vs 46 |
| M6 Unpinned deps | `paket.dependencies` | 7–10 |
| M9 Hardcoded version | `.github/workflows/general.yml` | 32, 50 |
