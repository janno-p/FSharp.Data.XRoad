# Loop Log

### Wave 1 — 2026-04-17
- T-001: XRoadMemberIdentifier equality — DONE. Files: FSharp.Data.XRoad.fs, CoreTypes.Tests.fs. Build P, Tests P(111). Next: T-006
- T-002: XRoadCentralServiceIdentifier equality — DONE. Files: FSharp.Data.XRoad.fs. Build P, Tests P. Next: T-011
- T-003: BinaryContent Create(Stream) — DONE. Files: FSharp.Data.XRoad.fs. Build P, Tests P. Next: T-008
- T-004: Custom attributes verified — DONE. Files: none changed (already complete). Tests P. Next: T-007
- T-005: Optional helpers verified — DONE. Files: none changed (already complete). Tests P. Next: T-007
- T-006: XRoadServiceIdentifier equality — DONE. Files: FSharp.Data.XRoad.fs. Build P, Tests P. Next: T-012
- T-007: IChoiceOf1-IChoiceOf8 verified — DONE. Files: CoreTypes.Tests.fs. Build P, Tests P.
- T-008: XRoadHeader.Id=getUUID(), ToString() — DONE. Files: FSharp.Data.XRoad.fs, CoreTypes.Tests.fs. Build P, Tests P.
- T-009: IXRoadHttpClientFactory + HttpClientFactory on endpoint — DONE. Files: FSharp.Data.XRoad.fs, FSharp.Data.XRoad.fsproj, DesignTime.fsproj. Build P, Tests P.
- T-010: openWsdlStream bounds check — DONE. Files: FSharp.Data.XRoad.MetaServices.fs. Build P, Tests P.
- T-011: TryParse(Result<T,string>) on all 3 identifier types — DONE. Files: FSharp.Data.XRoad.fs, CoreTypes.Tests.fs. Build P, Tests P(145).

### Wave 2 — 2026-04-17
- T-012: Request context and tracing — DONE. Files: FSharp.Data.XRoad.fs, FSharp.Data.XRoad.Protocol.fs, CoreTypes.Tests.fs. Build P, Tests P(151). All 12 tasks complete.

### Wave 3 — 2026-04-17
- T-013: Empty-field validation in Member/Subsystem/Service TryParse — DONE. Files: FSharp.Data.XRoad.fs, CoreTypes.Tests.fs. Build P, Tests P(157).
- T-014: Fix version regex ^v{\d+}$ → ^v\d+$; 5-part member-level parsing + tests — DONE. Files: FSharp.Data.XRoad.fs, CoreTypes.Tests.fs. Build P, Tests P(157).
- T-015: Remove duplicate ResponseReady trigger from MakeServiceCall — DONE. Files: FSharp.Data.XRoad.Protocol.fs. Build P, Tests P(157). All 15 tasks complete.

### Wave 4 — 2026-04-17
- T-016: Remove duplicate RequestReady trigger from MakeServiceCall (line 290) — DONE. Files: FSharp.Data.XRoad.Protocol.fs. Build P, Tests P(160).
- T-017: Add counter-based event tests (RequestReady 1×, ResponseReady 1×, multi-trigger) — DONE. Files: CoreTypes.Tests.fs. Build P, Tests P(160). All 17 tasks complete.

### Wave 5 — 2026-04-18
- T-018: Regression test via XRoadRequest.CreateMessage path — DONE. Files: CoreTypes.Tests.fs. Build P, Tests P(161). All 18 tasks complete.

### Wave 6 — 2026-04-18
- T-019: subsystemCode length guard fix — DONE. Files: FSharp.Data.XRoad.fs, CoreTypes.Tests.fs. Build P, Tests P(165). All 19 tasks complete.
