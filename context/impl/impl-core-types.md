---
created: "2026-04-17T00:00:00Z"
last_edited: "2026-04-17T00:00:00Z"
---
# Implementation Tracking: core-types

Build site: context/plans/build-site.md

| Task | Status | Notes |
|------|--------|-------|
| T-001 | DONE | Equality (Equals/GetHashCode/IEquatable/op_Equality) added to XRoadMemberIdentifier in FSharp.Data.XRoad.fs |
| T-002 | DONE | Equality added to XRoadCentralServiceIdentifier in FSharp.Data.XRoad.fs |
| T-003 | DONE | BinaryContent.Create(Stream) and Create(string,Stream) factory overloads added |
| T-004 | DONE | All 6 custom attributes verified complete in FSharp.Data.XRoad.Attributes.fs; tests in CoreTypes.Tests.fs |
| T-005 | DONE | OptionalHelpers.tryGetValue verified; Nullable<T>/Option<T> pattern tested |
| T-006 | DONE | Equality added to XRoadServiceIdentifier in FSharp.Data.XRoad.fs |
| T-007 | DONE | IChoiceOf1-IChoiceOf8 verified complete in FSharp.Data.XRoad.Choices.fs; tests added |
| T-008 | DONE | XRoadHeader: Id=getUUID(), ToString() added; 6 tests in CoreTypes.Tests.fs |
| T-009 | DONE | IXRoadHttpClientFactory interface + HttpClientFactory property on AbstractEndpointDeclaration; net472 System.Net.Http ref added |
| T-010 | DONE | openWsdlStream: bounds check added (failwith on empty Parts array) |
| T-011 | DONE | TryParse (Result<T,string>) added to all 3 identifier types; Parse delegates to TryParse; whitespace trimming; descriptive errors |
| T-012 | DONE | IXRoadRequest.RequestId (UUID per request); RequestReady/ResponseReady carry RequestId+ServiceCode+Version; MakeServiceCall triggers both events; 6 tests |
