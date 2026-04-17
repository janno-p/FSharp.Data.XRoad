---
created: "2026-04-17T00:00:00Z"
last_edited: "2026-04-17T00:00:00Z"
---

# Review Findings

| Finding | Severity | File | Status |
|---------|----------|------|--------|
| F-001: Service identifier version regex broken (`^v{\d+}$` should be `^v\d+$`) | P1 | src/FSharp.Data.XRoad/FSharp.Data.XRoad.fs:231 | NEW |
| F-002: ResponseReady event fired twice per service call (line 47 and 294) | P2 | src/FSharp.Data.XRoad/FSharp.Data.XRoad.Protocol.fs:47,294 | NEW |
| F-003: Empty required fields accepted in Member/Service TryParse (no length guards) | P1 | src/FSharp.Data.XRoad/FSharp.Data.XRoad.fs:158-164,229-236 | NEW |
| F-004: No test for 5-part member-level service identifier with version | P3 | tests/FSharp.Data.XRoad.Tests/FSharp.Data.XRoad.CoreTypes.Tests.fs | NEW |
| F-005: RequestReady event fired twice per service call (line 260 in CreateMessage and line 290 in MakeServiceCall) | P2 | src/FSharp.Data.XRoad/FSharp.Data.XRoad.Protocol.fs:260,290 | NEW |
| G-001: Tests don't verify single-fire semantics for RequestReady/ResponseReady events | P3 | tests/FSharp.Data.XRoad.Tests/FSharp.Data.XRoad.CoreTypes.Tests.fs:453-495 | NEW |

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
