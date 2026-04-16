---
created: "2026-04-16T00:00:00Z"
last_edited: "2026-04-16T00:00:00Z"
---

# Cavekit: Type Generation

## Scope

This cavekit covers the conversion of parsed WSDL/XSD schemas into ProvidedTypeDefinition trees that the F# type provider exposes to users. It includes:
- Mapping parsed complex types to CLR type definitions (properties, fields, methods)
- Handling inheritance and type extension
- Generating custom attributes (XRoadType, XRoadElement, XRoadOperation, etc.)
- Choice type composition and union handling
- Collection and array wrapping
- Nullable type representation
- Service operation signatures

This cavekit does NOT include IL emit for serialization (see cavekit-serialization) or the type provider infrastructure (see cavekit-type-provider).

## Requirements

### R1: Generate ProvidedTypeDefinition for Complex Types
**Description:** Parsed complex types must be converted to ProvidedTypeDefinition with properties/fields corresponding to schema elements.

**Acceptance Criteria:**
- [ ] Complex types with sequence composition generate a class with read-write properties for each element
- [ ] Complex types with choice composition generate a discriminated union representation
- [ ] Properties are generated with appropriate CLR types (Int32, String, DateTime, custom types)
- [ ] Properties have correct cardinality metadata (required, optional, repeated)
- [ ] XRoadElement custom attributes are applied to properties with proper configuration
- [ ] Type names are sanitized to avoid C# reserved keywords
- [ ] Namespace information is included in generated types
- [ ] Generated types are marked with [<Sealed>] or appropriate modifiers

**Dependencies:** cavekit-schema-parsing (R2), cavekit-core-types (R3 for custom attributes)

### R2: Handle Type Extension and Inheritance
**Description:** XSD types may extend or restrict other types. Inheritance relationships must be represented in generated code.

**Acceptance Criteria:**
- [ ] Complex types extending a base type inherit properties from the base
- [ ] Base type is included in generated type's base class reference
- [ ] Property order reflects extension (base properties first, derived properties last)
- [ ] Abstract base types are marked appropriately
- [ ] Multiple levels of inheritance are supported
- [ ] Name collisions between base and derived properties are detected and reported

**Dependencies:** R1 (type generation)

### R3: Generate Custom Attributes for Type Provider Integration
**Description:** Generated types must be annotated with custom attributes (XRoadType, XRoadElement, etc.) for runtime serialization logic.

**Acceptance Criteria:**
- [ ] [XRoadType(LayoutKind.Sequence)] is applied to sequence-based types
- [ ] [XRoadType(LayoutKind.Choice)] is applied to choice-based types
- [ ] [XRoadElement] is applied to element-like properties with name and namespace
- [ ] [XRoadOperation] is applied to service operation methods
- [ ] [XRoadRequest] and [XRoadResponse] are applied to SOAP message wrapper types
- [ ] [XRoadCollection] is applied to repeated element properties
- [ ] Attribute parameters (namespace, name, cardinality) are correctly set

**Dependencies:** cavekit-core-types (attributes), R1 (type generation)

### R4: Handle Nullable and Optional Elements
**Description:** Schema elements with minOccurs=0 or nillable=true must be represented as nullable.

**Acceptance Criteria:**
- [ ] Optional elements (minOccurs=0) are wrapped in Option<T> or Nullable<T>
- [ ] Nillable elements are wrapped in Option<T> or Nullable<T>
- [ ] Required elements (minOccurs > 0, nillable=false) are non-nullable
- [ ] Nullable<T> wrapper is preferred for value types (int, DateTime)
- [ ] Option<T> wrapper is preferred for reference types (string, custom types)
- [ ] Property initialization for optional properties is set to None or null
- [ ] Generated code compiles without null-reference errors

**Dependencies:** R1 (type generation)

### R5: Generate Collection Types for Repeated Elements
**Description:** Schema elements with maxOccurs > 1 must be represented as collections.

**Acceptance Criteria:**
- [ ] Repeated elements are wrapped in List<T> or array representation
- [ ] Collection initialization (empty list, null) is correct
- [ ] Collection types are marked with [XRoadCollection] attribute
- [ ] Element cardinality (minOccurs, maxOccurs) is encoded in the attribute
- [ ] Mixed lists (unbounded, bounded) are handled uniformly
- [ ] Collections of primitive types are supported
- [ ] Collections of complex types are supported
- [ ] Nested collections (list of lists) are not used (flattened if necessary)

**Dependencies:** R1 (type generation)

### R6: Generate Service Operation Signatures
**Description:** WSDL operations must be converted to F# function signatures in service type definitions.

**Acceptance Criteria:**
- [ ] WSDL operation maps to a method with the operation name
- [ ] Request message parts become method parameters
- [ ] Response message becomes the return type
- [ ] One-way operations (no response) return unit
- [ ] Faults/exceptions are represented as F# choice types or exceptions
- [ ] Method is marked with [XRoadOperation] attribute
- [ ] Async variants of operations are available if configured

**Dependencies:** R1, cavekit-core-types

### R7: Handle Choice Types and Discriminated Unions
**Description:** XSD choice elements (one of multiple alternatives) must be represented as discriminated unions.

**Acceptance Criteria:**
- [ ] XSD choice groups generate an F# discriminated union (choice type)
- [ ] Each choice alternative becomes a union case
- [ ] Union cases have appropriate names (sanitized from XSD element names)
- [ ] Union cases with data include the element's type
- [ ] Empty choice elements are represented as nullary cases
- [ ] Properties with choice type are properly initialized
- [ ] Choice unions are wrapped in an interface for polymorphism if needed (see cavekit-core-types)

**Dependencies:** R1, cavekit-core-types (choice wrapper)

### R8: Namespace and Naming Conventions [GAP]
**Description:** Generated type names must follow conventions and avoid conflicts.

**Acceptance Criteria:**
- [ ] Type names are derived from XSD type name (e.g., "PersonInfo" from xs:PersonInfo)
- [ ] Type names are sanitized (spaces, hyphens, dots converted to underscores or removed)
- [ ] C# and F# reserved keywords are escaped (e.g., "type" becomes "type_")
- [ ] Nested type names include parent scope (e.g., "PersonInfo_AddressType")
- [ ] Namespace prefixes are included in fully qualified names
- [ ] Name collisions between sibling types are detected and reported
- [ ] Generated names are deterministic (same schema always produces same names)

**Dependencies:** None (naming convention)

**Status:** [GAP] Current code: No explicit keyword escaping. Naming logic at lines 1500+ in Builder.fs. Limited collision detection.

### R9: Handle Type Aliasing and Substitution Groups
**Description:** XSD substitution groups and type restrictions may create type aliases or alternatives.

**Acceptance Criteria:**
- [ ] Substitution groups are recognized and expanded
- [ ] Type restrictions generate derived types or validators
- [ ] Aliases are resolved to actual types (not duplicated)
- [ ] Substitution element alternatives are listed in metadata
- [ ] Generated code is simplified if substitution requires no special handling

**Dependencies:** R2 (inheritance)

### R10: Type Generation Thread Safety [GAP]
**Description:** Type generation may occur in multiple contexts; generated state must be thread-safe.

**Acceptance Criteria:**
- [ ] TypeGenerator state (mutable ResizeArray, counters) is not shared across threads
- [ ] ProvidedTypeDefinition trees are not modified after initial construction
- [ ] Type cache (if any) is properly synchronized
- [ ] Concurrent type generation requests do not produce corrupted output

**Dependencies:** None (concurrency concern)

**Status:** [GAP] Current code: Builder.fs lines 1480+ use mutable ResizeArray without synchronization. Type caching in type provider may have race conditions.

### R11: Generate Validation Helpers [GAP] (Optional)
**Description:** Generated types may include validation methods for constraint checking.

**Acceptance Criteria:**
- [ ] Properties with length restrictions include validation
- [ ] Properties with pattern restrictions include validation
- [ ] Properties with enumeration restrictions include validation
- [ ] Validation methods are called automatically (or optionally) on assignment
- [ ] Validation errors are reported with clear messages

**Dependencies:** cavekit-schema-parsing (constraint extraction)

**Status:** [GAP] Current code: No validation helpers generated. Constraints are known but unused.

## Out of Scope

- IL emit for serialization delegates (see cavekit-serialization)
- ProvidedTypes SDK details (quotation syntax, provided method implementation)
- Type caching and type provider context management (see cavekit-type-provider)
- Schema document fetching (see cavekit-http-transport)
- Schema parsing (see cavekit-schema-parsing)

## Cross-References

- **cavekit-schema-parsing**: Provides parsed schema input to type generation
- **cavekit-serialization**: Generated types are serialized via IL emit
- **cavekit-core-types**: Custom attributes applied to generated types
- **cavekit-type-provider**: Type generation is invoked by the type provider infrastructure

## Source Traceability

**Source files:**
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Builder.fs` (1706 lines) — Schema to ProvidedTypes conversion

**Related files:**
- `src/FSharp.Data.XRoad.DesignTime/FSharp.Data.XRoad.Schema.fs` — Input schema model
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Attributes.fs` — Custom attributes used in generated code

**Typical flow:**
1. Schema parsing produces Schema.TypeDefinition tree (cavekit-schema-parsing)
2. Builder module converts to ProvidedTypeDefinition tree
3. Type provider exposes the tree to F# compiler
4. Generated types are annotated with custom attributes
5. At runtime, attributes guide serialization (cavekit-serialization)
