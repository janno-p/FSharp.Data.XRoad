---
created: "2026-04-18T00:00:00Z"
last_edited: "2026-04-18T00:00:00Z"
---
# Implementation Tracking: protocol

Build site: context/plans/build-site-protocol.md

| Task | Status | Notes |
|------|--------|-------|
| T-001 | DONE | SOAP 1.1 envelope construction verified: namespace, Header/Body, protocolVersion, UUID id, client/service elements, xro/id namespace declarations, UTF-8, optional subsystem/version — 11 tests |
| T-002 | DONE | Client cert config verified: AuthenticationCertificates, AcceptedServerCertificate on AbstractEndpointDeclaration, cert attachment to HttpWebRequest — 3 tests |
| T-003 | DONE | HTTP POST behavior verified: Method=POST, Content-Type, SOAPAction="", URI, body contains SOAP XML — 4 tests |
| T-004 | TODO | |
| T-005 | DONE | Server cert validation behavior verified: no custom callback without AcceptedServerCertificate (default system validation), AcceptedServerCertificate settable for pinning — 2 tests |
| T-006 | DONE | Added Timeout property (default 30000ms) to AbstractEndpointDeclaration; wired request.Timeout in XRoadRequest constructor — 3 tests |
| T-007 | DONE | Resource cleanup verified: XRoadRequest implements IDisposable, use binding works — 2 tests |
| T-008 | TODO | |
| T-009 | TODO | |
| T-010 | TODO | |
| T-011 | TODO | |
| T-012 | TODO | |
| T-013 | TODO | |
