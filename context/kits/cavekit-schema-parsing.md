---
created: "2026-04-16T00:00:00Z"
last_edited: "2026-04-16T00:00:00Z"
---

# Cavekit: WSDL & Schema Parsing

## Scope

This cavekit covers parsing of WSDL (Web Services Description Language) and XSD (XML Schema Definition) documents into an intermediate representation suitable for type generation. It includes:
- WSDL document model (operations, bindings, ports, messages, types)
- XSD type system (complex types, simple types, elements, groups, attributes, extensions, restrictions)
- XML namespace handling and qualified name resolution
- Type mapping (XSD type names to intermediate representation)
- Schema composition (imports, includes, references)
- Whitespace restrictions and simple type facets
- SOAP encoding and binding format detection

This cavekit does NOT include HTTP fetching (see cavekit-http-transport) or type code generation (see cavekit-type-generation).

## Requirements

### R1: Parse WSDL Document Structure
**Description:** WSDL documents must be parsed into a navigable object model representing services, operations, bindings, and port types.

**Acceptance Criteria:**
- [ ] WSDL root element and namespace are recognized
- [ ] WSDL types section is located and parsed (contains XSD schemas)
- [ ] WSDL message definitions are extracted with message name and parts
- [ ] WSDL portType definitions are extracted with operations
- [ ] WSDL binding definitions are extracted with transport protocol (SOAP) information
- [ ] WSDL service definitions are extracted with ports and endpoint URIs
- [ ] Qualified names (QNames) are resolved using namespace declarations in scope
- [ ] Missing required WSDL elements fail with a clear error

**Dependencies:** None (foundational)

### R2: Parse XSD Complex Types
**Description:** Complex type definitions from XSD must be parsed into an intermediate representation for field/property generation.

**Acceptance Criteria:**
- [ ] Complex types with sequence composition are parsed
- [ ] Complex types with choice composition are parsed
- [ ] Complex types with all composition are parsed
- [ ] Complex types using extension (inheritance) are parsed
- [ ] Complex types using restriction are parsed
- [ ] Complex type attributes (abstract, mixed, final, block) are recognized
- [ ] Abstract complex types are marked for later handling
- [ ] Mixed content types are supported
- [ ] Complex types with no explicit composition default to sequence
- [ ] Duplicate definitions of the same type fail with a clear error

**Dependencies:** None (orthogonal)

### R3: Parse XSD Simple Types
**Description:** Simple type definitions (primitives, lists, unions, restricted types) must be parsed.

**Acceptance Criteria:**
- [ ] Simple types referencing built-in XSD types are recognized
- [ ] Simple types using restriction (with facets like minLength, maxLength, pattern, enumeration) are parsed
- [ ] Simple type enumerations are extracted as a list of allowed values
- [ ] Simple type pattern restrictions are extracted as regex
- [ ] Simple type numeric restrictions (minInclusive, maxInclusive, minExclusive, maxExclusive) are extracted
- [ ] Simple type length restrictions (minLength, maxLength, length) are extracted
- [ ] Simple types using list composition are recognized
- [ ] Simple types using union composition are recognized
- [ ] Whitespace restrictions (collapse, preserve, replace) are extracted

**Dependencies:** None (orthogonal)

### R4: Parse XSD Elements and Attributes
**Description:** Element and attribute declarations must be parsed with their types, cardinality, and restrictions.

**Acceptance Criteria:**
- [ ] Global element declarations are extracted with name, type, and namespace
- [ ] Local element declarations within complex types are extracted with name and type
- [ ] Element cardinality (minOccurs, maxOccurs) is extracted
- [ ] Element nillability (nillable attribute) is extracted
- [ ] Element default values are extracted
- [ ] Element fixed values are extracted
- [ ] Attribute declarations are extracted with name, type, use (required/optional/prohibited), and default values
- [ ] Element references (ref attribute) are resolved to referenced element declarations
- [ ] Attribute references (ref attribute) are resolved to referenced attribute declarations

**Dependencies:** R2, R3 (types referenced by elements/attributes)

### R5: Parse XSD Groups and Attribute Groups
**Description:** Groups (model groups, attribute groups) must be parsed and their members extracted.

**Acceptance Criteria:**
- [ ] Global model group definitions are extracted
- [ ] Local model groups (sequence, choice, all) within complex types are extracted
- [ ] Global attribute group definitions are extracted
- [ ] Group cardinality (minOccurs, maxOccurs) is applied to group members
- [ ] Group references (ref attribute) are resolved
- [ ] Nested groups are flattened or properly composed

**Dependencies:** R2, R4 (groups contain elements/attributes)

### R6: Parse WSDL Type Mappings (XSD → CLR)
**Description:** XSD built-in types must map to appropriate CLR types (String, Int32, DateTime, etc.).

**Acceptance Criteria:**
- [ ] xs:string maps to System.String
- [ ] xs:int, xs:integer, xs:long, xs:short, xs:byte map to integer types
- [ ] xs:decimal maps to System.Decimal
- [ ] xs:double, xs:float map to floating-point types
- [ ] xs:boolean maps to System.Boolean
- [ ] xs:date maps to NodaTime.OffsetDate
- [ ] xs:dateTime maps to NodaTime.OffsetDateTime
- [ ] xs:time maps to NodaTime.OffsetTime
- [ ] xs:duration maps to NodaTime.Period
- [ ] xs:base64Binary maps to byte array
- [ ] xs:hexBinary maps to byte array
- [ ] Unknown XSD types fail with a clear error listing the unhandled type

**Dependencies:** None (mapping table)

### R7: Handle Schema Composition (imports, includes)
**Description:** Schemas may reference external schemas via import/include. These must be resolved.

**Acceptance Criteria:**
- [ ] xs:import elements are parsed to locate external schemas
- [ ] xs:include elements are parsed to locate schemas in the same namespace
- [ ] Referenced schemas are fetched (by HTTP transport or local file system)
- [ ] Imported types are merged into a single type namespace
- [ ] Namespace prefixes in imports are resolved correctly
- [ ] Circular imports are detected and reported with a clear error
- [ ] Missing imported schemas fail with a clear error (attempted fetch location)

**Dependencies:** HTTP transport (external schema fetching)

### R8: Error Handling and Diagnostics [GAP]
**Description:** Schema parsing errors must be collected and reported without stopping at the first error.

**Acceptance Criteria:**
- [ ] Unsupported schema constructs (annotations, wildcards, groups not yet supported) are collected as warnings or errors
- [ ] Multiple errors are accumulated and reported together
- [ ] Error messages include the schema element location (line number, element name, namespace)
- [ ] Warnings are distinguished from errors
- [ ] Parsing completes with best-effort output even if some constructs are unsupported
- [ ] Fail-fast (failwith) behavior is replaced with error collection
- [ ] Circular references and recursion are detected

**Dependencies:** None (orthogonal)

**Status:** [GAP] Current code: Uses `failwith` extensively (lines 45, 49, 62, 75, 90, etc.). No error accumulation. Stops on first error.

### R9: Handle Nested Schema Complexity [GAP]
**Description:** Deeply nested schemas must not cause stack overflow or exponential type generation.

**Acceptance Criteria:**
- [ ] Recursion depth limit is enforced during schema parsing (e.g., max 100 levels)
- [ ] Recursive type definitions are detected (type references itself transitively)
- [ ] Parsing fails with a clear error if recursion limit is exceeded
- [ ] Parsing fails with a clear error if recursive types are detected

**Dependencies:** None (robustness)

**Status:** [GAP] Current code: No recursion depth tracking. Recursive schema definitions could cause stack overflow.

### R10: Preserve Semantic Information
**Description:** Schema parsing must capture all information needed by type generation without loss.

**Acceptance Criteria:**
- [ ] Type annotations (documentation) are captured
- [ ] Type origin (source WSDL, source XSD) is tracked
- [ ] Namespace information is preserved
- [ ] Type cardinality information is preserved
- [ ] Restriction and facet information is preserved
- [ ] Default and fixed values are preserved
- [ ] Abstract and final modifiers are preserved

**Dependencies:** None (data preservation)

### R11: Handle SOAP Encoding
**Description:** WSDL messages may use SOAP encoding (ArrayType attributes, compound types) which requires special handling.

**Acceptance Criteria:**
- [ ] SOAP-encoded array types (with ArrayType attribute) are recognized
- [ ] SOAP-encoded array types are converted to native sequence/array representation
- [ ] SOAP-encoded compound types are treated as regular complex types
- [ ] SOAP namespace prefixes are handled correctly

**Dependencies:** R2, R3 (type parsing)

## Out of Scope

- Validation of XSD constraints (uniqueness, keys, foreign keys) — these are informational only
- DTD processing — only XSD schemas are supported
- RelaxNG or other schema formats
- WSDL 2.0 (only WSDL 1.1 is supported)
- Code generation and type construction (see cavekit-type-generation)
- Serialization/deserialization logic (see cavekit-serialization)

## Cross-References

- **cavekit-http-transport**: Fetches WSDL and XSD documents over HTTP
- **cavekit-type-generation**: Uses parsed schema to generate CLR types
- **cavekit-serialization**: Uses schema information to generate serialization IL

## Source Traceability

**Source files:**
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Xml.fs` (38 lines) — Low-level XML helpers
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Wsdl.fs` (491 lines) — WSDL document model and parsing
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Schema.fs` (1013 lines) — XSD schema parsing and type definitions

**Typical usage:**
1. HTTP transport fetches WSDL → XDocument (cavekit-http-transport)
2. Wsdl.fs parses XDocument into WSDL model
3. Schema.fs parses XSD types nested in WSDL
4. Builder.fs consumes parsed schema to generate types (see cavekit-type-generation)
