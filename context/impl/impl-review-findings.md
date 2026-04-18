---
created: "2026-04-17T00:00:00Z"
last_edited: "2026-04-18T14:30:00Z"
---


# Review Findings

| Finding | Severity | File | Status |
|---------|----------|------|--------|
| F-001: Service identifier version regex broken (`^v{\d+}$` should be `^v\d+$`) | P1 | src/FSharp.Data.XRoad/FSharp.Data.XRoad.fs:231 | FIXED (T-014) |
| F-002: ResponseReady event fired twice per service call (line 47 and 294) | P2 | src/FSharp.Data.XRoad/FSharp.Data.XRoad.Protocol.fs:47,294 | FIXED (T-015) |
| F-003: Empty required fields accepted in Member/Service TryParse (no length guards) | P1 | src/FSharp.Data.XRoad/FSharp.Data.XRoad.fs:158-164,229-236 | FIXED (T-013) |
| F-004: No test for 5-part member-level service identifier with version | P3 | tests/FSharp.Data.XRoad.Tests/FSharp.Data.XRoad.CoreTypes.Tests.fs | FIXED (T-014) |
| F-005: RequestReady event fired twice per service call (line 260 in CreateMessage and line 290 in MakeServiceCall) | P2 | src/FSharp.Data.XRoad/FSharp.Data.XRoad.Protocol.fs:260,290 | FIXED (T-016) |
| G-001: Tests don't verify single-fire semantics for RequestReady/ResponseReady events | P3 | tests/FSharp.Data.XRoad.Tests/FSharp.Data.XRoad.CoreTypes.Tests.fs:453-495 | FIXED (T-017) |
| F-006: T-017 counter tests call TriggerRequestReady directly — don't protect against MakeServiceCall regression | P1 | tests/FSharp.Data.XRoad.Tests/FSharp.Data.XRoad.CoreTypes.Tests.fs:501-534 | FIXED (T-018) |
| F-007: Missing subsystemCode length guard in SUBSYSTEM/SERVICE TryParse — empty subsystemCode accepted | P1 | src/FSharp.Data.XRoad/FSharp.Data.XRoad.fs:163,233,235 | FIXED (T-019) |
| F-008: Unused XRoadHeader copy constructor with shallow Unresolved copy — dead code, YAGNI violation | P2 | src/FSharp.Data.XRoad/FSharp.Data.XRoad.fs:275-287 | NEW |
| F-PF-009: checkFaultInStream uses default XmlReader.Create(stream) — missing CloseInput=false, asymmetric stream ownership vs parseSoapEnvelopeBody | P2 | src/FSharp.Data.XRoad/FSharp.Data.XRoad.Protocol.fs:34 | NEW |
| F-PF-010: serverError ref captured but never checked in test helpers — server thread failure causes 30s hang | P3 | tests/FSharp.Data.XRoad.Tests/FSharp.Data.XRoad.Protocol.Tests.fs:581,698 | NEW |
| F-PF-011: FS0760 warning — TcpListener constructed without `new` keyword in test (IDisposable intent) | P3 | tests/FSharp.Data.XRoad.Tests/FSharp.Data.XRoad.Protocol.Tests.fs:660 | NEW |

## Details

### F-001 (P1): Service identifier version regex broken
- Pattern `^v{\d+}$` — `{` is literal in .NET regex when not a valid quantifier.
- `"v1"` does NOT match; 5-part SERVICE:EE/GOV/123/getSomething/v1 falls into subsystem branch.
- Result: ServiceVersion silently lost, "getSomething" becomes SubsystemCode, "v1" becomes ServiceCode.
- Fix: `src/FSharp.Data.XRoad/FSharp.Data.XRoad.fs:231` — change `@"^v{\d+}$"` to `@"^v\d+$"`
- Task: T-014

### F-002 (P2): Duplicate ResponseReady event
- `XRoadResponse.RetrieveMessage()` fires at line 47.
- `XRoadUtil.MakeServiceCall()` fires again at line 294 after RetrieveMessage() returns.
- Subscribers get two ResponseReady calls per request.
- Fix: Remove line 294 from MakeServiceCall.
- Task: T-015

### F-003 (P1): Empty fields accepted in Member/Service TryParse
- `TryParse("MEMBER:EE//code")` returns Ok with MemberClass="".
- XRoadCentralServiceIdentifier.TryParse has guards (line 92), but Member (158-164) and Service (229-236) don't.
- Fix: Add `when xRoadInstance.Length > 0 && memberClass.Length > 0 && memberCode.Length > 0` guards.
- Task: T-013

### F-004 (P3): No test for 5-part versioned service identifier
- Tests cover 4-part (no version) and 6-part (subsystem+version) but not 5-part (member-level+version).
- This masked F-001 — the broken code path had no test.
- Fix: Add test `XRoadServiceIdentifier.TryParse("SERVICE:EE/GOV/70000001/getService/v1")` asserting ServiceVersion="v1", Owner.SubsystemCode="".
- Task: T-014 (combined with F-001 fix)

### F-005 (P2): Duplicate RequestReady event
- `XRoadRequest.CreateMessage()` fires at line 260.
- `XRoadUtil.MakeServiceCall()` fires again at line 290 after CreateMessage() returns.
- Subscribers get two RequestReady calls per request. Mirrors the F-002 pattern fixed for ResponseReady (T-015).
- Fix: Remove line 290 from MakeServiceCall — natural fire point is CreateMessage.
- Task: T-016

### G-001 (P3): Tests don't verify event single-fire semantics
- RequestContextTracingTests (lines 453-495) checks event args structure only, not fire count.
- This is why F-002 and F-005 weren't caught by tests; inspection found both.
- Fix: Add counter-based tests asserting RequestReady fires 1× and ResponseReady fires 1× per MakeServiceCall.
- Task: T-017

### F-006 (P1): T-017 counter tests don't protect against MakeServiceCall regression
- Tests call TriggerRequestReady directly on endpoint. Verify event mechanism in isolation only.
- Would pass unchanged if duplicate trigger were re-added to MakeServiceCall (line 290 Protocol.fs).
- Fix: Add test that exercises the CreateMessage path (accessible via InternalsVisibleTo) and counts RequestReady fires through the full MakeServiceCall-adjacent flow. Or add a structural assertion counting TriggerRequestReady call sites in MakeServiceCall.
- Task: T-018

## Protocol Domain Findings (2026-04-18 ck:check)

| Finding | Severity | File | Status |
|---------|----------|------|--------|
| PF-001: parseSoapEnvelopeBody returns XmlReader that closes underlying stream — double-dispose | P1 | Protocol.fs:24-30,67 | FIXED (T-014) |
| PF-002: Fault fallback at line 74 raises generic Exception, not XRoadFault — inconsistent type | P2 | Protocol.fs:73-74 | FIXED (T-015) |
| PF-003: checkFaultInStream uses InnerXml instead of Value — may return XML markup in fault message | P2 | Protocol.fs:42 | FIXED (T-015) |
| PF-004: Port TOCTOU race in integration tests — TcpListener released before HttpListener binds | P2 | Protocol.Tests.fs:536-541 | FIXED (T-016) |
| PF-005: Silent `with _ -> ()` in test server thread — failures hidden as timeouts | P2 | Protocol.Tests.fs:553 | FIXED (T-016) |
| PF-006: R6.e not tested — ResponseReady on SOAP fault path not covered | P3 | Protocol.Tests.fs | FIXED (T-017) |
| PF-007: R13 "configurable" criterion mismatched — event-based design, not file/debug-mode config | P3 | cavekit-protocol.md:R13 | FIXED (kit updated) |
| PF-OVER-001: RequestReady event has no cavekit requirement — over-built | — | Protocol.fs:267; FSharp.Data.XRoad.fs:404 | FIXED (R14 added to kit) |

### PF-001 (P1): Double-dispose in parseSoapEnvelopeBody
- `XmlReader.Create(stream)` disposes underlying stream when reader is disposed (default behavior).
- `use reader = parseSoapEnvelopeBody content` disposes `content` stream at scope exit.
- `use content = ...` also disposes `content` at method end → double-dispose.
- For MemoryStream this is safe (idempotent Dispose) but ownership is wrong; breaks if stream type changes.
- Fix: `XmlReader.Create(stream, XmlReaderSettings(CloseInput = false))` in parseSoapEnvelopeBody.
- Task: T-014

### PF-002 (P2): Fault fallback raises wrong exception type
- `checkXRoadFault` at line 65 raises `XRoadFault` for SOAP faults.
- Fallback at line 74: `failwith "Request resulted an error: ..."` raises generic `Exception`.
- If XPath detection misses an edge case, caller catching `XRoadFault` won't catch this fallback.
- Fix: Change line 74 to `raise (XRoadFault("", reader.ReadInnerXml()))`.
- Task: T-015

### PF-003 (P2): InnerXml instead of Value for fault message
- `nodeToString` uses `.InnerXml` (line 42) — returns XML-serialized content including markup.
- `faultString` should be plain text; `.Value` returns text nodes only.
- Fix: Change `InnerXml` to `Value` in nodeToString lambda (Protocol.fs:42).
- Task: T-015 (combined with PF-002)

### PF-004 (P2): Port TOCTOU race in integration tests
- `TcpListener(0)` finds free port; `.Stop()` releases it; then `HttpListener.Start()` tries to bind it.
- Window between Stop() and Start() allows another process/test to grab the port.
- Fix: Retry logic on HttpListener.Start(), or use a global thread-safe port counter.
- Task: T-016

### PF-005 (P2): Silent exception in test server thread
- Background thread: `with _ -> ()` swallows all exceptions.
- If listener fails, test client hangs 30 seconds (default Timeout) before timing out.
- Fix: Capture exception in shared ref; check before sending HTTP request.
- Task: T-016 (combined with PF-004)

### PF-006 (P3): R6.e missing test coverage
- R6.e: "Event is raised even if response is a SOAP Fault"
- ResponseReady fires at line 57 BEFORE checkFaultInStream at line 65 — correctly implemented.
- No test verifies this path (fault response + ResponseReady still fired).
- Fix: Add integration test: HttpListener returns SOAP Fault; verify ResponseReady fires before XRoadFault raised.
- Task: T-017

### F-007 (P1): Missing subsystemCode length guard in SUBSYSTEM/SERVICE TryParse
- `XRoadMemberIdentifier.TryParse("SUBSYSTEM:EE/GOV/123/")` returns Ok with SubsystemCode="" — should return Error.
- `XRoadServiceIdentifier.TryParse("SERVICE:EE/GOV/123//getSomething")` returns Ok with SubsystemCode="" — should return Error.
- Line 163: 4-part SUBSYSTEM guard missing `&& subsystemCode.Length > 0`.
- Lines 233, 235: 5/6-part SERVICE-with-subsystem guards missing `&& subsystemCode.Length > 0`.
- Fix: Add `&& subsystemCode.Length > 0` to when guards on lines 163, 233, 235 of FSharp.Data.XRoad.fs.
- Task: T-019

## Protocol Domain Findings — 2nd check (2026-04-18)

### F-PF-009 (P2): checkFaultInStream missing CloseInput=false
- `parseSoapEnvelopeBody` (line 25) correctly uses `XmlReaderSettings(CloseInput = false)` after T-014.
- `checkFaultInStream` (line 34) still uses `XmlReader.Create(stream)` — default closes stream on reader dispose.
- On fault path: `use reader = XmlReader.Create(stream)` disposes stream before exception propagates. Safe now (MemoryStream idempotent, no later access), but asymmetric ownership is a latent hazard.
- Fix: `XmlReader.Create(stream, XmlReaderSettings(CloseInput = false))` at Protocol.fs:34.

### F-PF-010 (P3): serverError ref captured but never checked in test helpers
- `startSoapServer` captures server thread exception in `mutable serverError: exn option` but never checks it before assertions.
- If listener thread fails, test client hangs 30s on HTTP timeout rather than getting a clear error message.
- Fix: Check `serverError` (with short delay) after `t.Start()` before issuing HTTP request, or expose ref to callers.

### F-PF-011 (P3): FS0760 compiler warning in test helper
- `System.Net.Sockets.TcpListener(...)` at Protocol.Tests.fs:660 constructed without `new` keyword.
- F# compiler warns: "objects supporting IDisposable should use `new Type(args)` syntax".
- Fix: `let tmp = new System.Net.Sockets.TcpListener(Net.IPAddress.Loopback, 0)` (3 occurrences in file).
