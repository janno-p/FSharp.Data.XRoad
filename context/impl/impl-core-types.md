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
| T-008 | TODO | XRoadHeader — depends on T-001, T-006 |
| T-009 | TODO | AbstractEndpointDeclaration — depends on T-001, T-006 |
| T-010 | TODO | MetaServices WSDL bootstrapping — depends on T-001, T-006 |
| T-011 | TODO | Identifier parsing robustness — depends on T-001, T-002, T-006 |
| T-012 | TODO | Request context and tracing — depends on T-008, T-009 |
