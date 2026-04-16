---
created: "2026-04-16T00:00:00Z"
last_edited: "2026-04-16T00:00:00Z"
---

# Cavekit: Serialization & IL Emit

## Scope

This cavekit covers runtime IL (intermediate language) code generation for SOAP/XML serialization and deserialization. It includes:
- IL generation for serializer and deserializer delegates
- Type mapping registry (CLR types to IL-emitted serialization code)
- Handling of primitive types, complex types, arrays, nullable types, and choice types
- Null/nil value handling in SOAP encoding
- Custom attribute interpretation (XRoadType, XRoadElement, XRoadCollection)
- MTOM (Message Transmission Optimization Mechanism) attachment handling
- Base64 encoding/decoding for binary content

This cavekit does NOT include HTTP transport (cavekit-protocol), WSDL parsing (cavekit-schema-parsing), or type generation (cavekit-type-generation).

## Requirements

### R1: Generate Serialization Delegates
**Description:** At runtime, IL-emitted delegates serialize CLR objects to XML/SOAP format.

**Acceptance Criteria:**
- [ ] Serializer delegates accept an object instance and write XML to XmlWriter
- [ ] Serializer delegates are cached in a TypeMap registry by CLR type
- [ ] First invocation generates and compiles IL, subsequent calls reuse compiled delegate
- [ ] Serializers handle root-level SOAP Body wrapper (properly named, namespaced)
- [ ] Serializers write type-specific namespace declarations
- [ ] Serialized output is valid XML (proper escaping, CDATA for special content)

**Dependencies:** cavekit-core-types (custom attributes)

### R2: Generate Deserialization Delegates
**Description:** At runtime, IL-emitted delegates deserialize XML/SOAP format to CLR objects.

**Acceptance Criteria:**
- [ ] Deserializer delegates accept an XmlReader and parse elements into CLR objects
- [ ] Deserializer delegates are cached in a TypeMap registry by CLR type
- [ ] First invocation generates and compiles IL, subsequent calls reuse compiled delegate
- [ ] Deserializers navigate XML structure based on element names and namespaces
- [ ] Deserializers construct objects with proper property/field initialization
- [ ] Deserializers handle missing required elements with clear errors
- [ ] Deserializers skip unknown elements without error (forward compatibility)

**Dependencies:** cavekit-core-types (custom attributes)

### R3: Handle Primitive Types (String, Int, Boolean, Decimal, Float)
**Description:** Built-in CLR types must be serialized and deserialized correctly.

**Acceptance Criteria:**
- [ ] System.String is serialized as text content, escaped for XML
- [ ] System.Int32, Int64, Int16, Int8 are serialized as decimal string
- [ ] System.Decimal is serialized with culture-invariant formatting
- [ ] System.Double and System.Single are serialized with appropriate precision
- [ ] System.Boolean is serialized as "true" or "false"
- [ ] Byte arrays are serialized as base64 or hex encoding
- [ ] Deserialization parses string values to primitive types with error on invalid format
- [ ] Type-specific parsing respects culture-invariant formats (no locale-specific parsing)

**Dependencies:** None (foundational)

### R4: Handle NodaTime Types (OffsetDate, OffsetDateTime, OffsetTime, Period)
**Description:** NodaTime date/time types are serialized in ISO 8601 format with timezone information.

**Acceptance Criteria:**
- [ ] NodaTime.OffsetDate is serialized as YYYY-MM-DD+HH:MM (with offset, no time)
- [ ] NodaTime.OffsetDateTime is serialized as YYYY-MM-DDTHH:MM:SS+HH:MM (with offset)
- [ ] NodaTime.OffsetTime is serialized as HH:MM:SS+HH:MM (with offset, no date)
- [ ] NodaTime.Period is serialized as ISO 8601 duration (PnDTnHnMnS format)
- [ ] Deserialization parses ISO 8601 strings back to NodaTime types
- [ ] Timezone offset is preserved during round-trip
- [ ] Invalid date/time strings fail with clear error message

**Dependencies:** None (type-specific serialization)

### R5: Handle Complex Types (Objects with Properties)
**Description:** User-defined classes with properties are serialized as XML elements with sub-elements.

**Acceptance Criteria:**
- [ ] Class properties are written as XML child elements with property name as element name
- [ ] Nested complex types are recursively serialized
- [ ] Required properties (non-nullable) are always written
- [ ] Optional properties (nullable, minOccurs=0) are written only if non-null
- [ ] Property order follows XSD definition order (from XRoadType attribute)
- [ ] Namespace is applied to elements based on XRoadElement attribute
- [ ] Deserialization reads elements in any order (order-independent)
- [ ] Deserialization builds object step-by-step as elements are encountered

**Dependencies:** R1, R2, cavekit-core-types (R3 for XRoadType/XRoadElement)

### R6: Handle Collections (List, Array, Repeated Elements)
**Description:** Collections of items are serialized as multiple XML elements with the same name.

**Acceptance Criteria:**
- [ ] List<T> is serialized as repeated child elements (e.g., <item/> <item/> <item/>)
- [ ] Arrays are serialized as repeated elements
- [ ] Empty collections are serialized as zero elements (no wrapper)
- [ ] Single-item collections are serialized as one element
- [ ] Element name is derived from XRoadCollection attribute or property name
- [ ] Collection item type is determined from generic parameter or attribute
- [ ] Deserialization reads all consecutive elements with matching name into a list
- [ ] Deserialization stops when a different element name is encountered

**Dependencies:** R5, cavekit-core-types (R4 for XRoadCollection)

### R7: Handle Nullable and Optional Values
**Description:** Values that can be null (Option<T>, Nullable<T>, nullable reference types) must be handled correctly.

**Acceptance Criteria:**
- [ ] Nullable<T> with no value is serialized with xsi:nil="true" attribute
- [ ] Nullable<T> with value is serialized with value (no nil attribute)
- [ ] Option<T> with None is serialized with xsi:nil="true"
- [ ] Option<T> with Some value is serialized with value
- [ ] Null reference types are serialized with xsi:nil="true"
- [ ] Deserialization creates Nullable<T> or Option<T> with appropriate value
- [ ] Deserialization of nil="true" creates empty nullable/option
- [ ] Deserialization of nil="false" ignores nil attribute

**Dependencies:** R1, R2, R3, R4, R5

### R8: Handle Choice Types (Discriminated Unions)
**Description:** XSD choice elements (pick one of several alternatives) are represented as discriminated unions.

**Acceptance Criteria:**
- [ ] Choice types are serialized by writing the selected alternative as a child element
- [ ] Only one alternative is written (choice constraint enforced)
- [ ] Element name determines which alternative was selected
- [ ] Deserialization reads one element and constructs the corresponding union case
- [ ] Union case name matches element name (sanitized if necessary)
- [ ] Choice types with data include the element's value in the union case

**Dependencies:** R5, R6, cavekit-core-types (R7 choice wrapper)

### R9: Handle MTOM and Binary Attachments
**Description:** Binary content can be optimized using MTOM (XOP include references).

**Acceptance Criteria:**
- [ ] Binary elements can be marked for MTOM optimization via attribute
- [ ] MTOM serializer writes xop:Include element with href to attachment content ID
- [ ] Binary content is added to multipart message attachments
- [ ] Attachment stream is read and encoded appropriately
- [ ] MTOM deserializer reads xop:Include reference and retrieves attachment
- [ ] Attachment content is returned as byte array or BinaryContent object
- [ ] Non-MTOM mode serializes binary as base64-encoded XML element

**Dependencies:** R3 (byte array), cavekit-protocol (multipart handling)

### R10: TypeMap Registry Thread Safety [GAP]
**Description:** Generated IL delegates are cached in a TypeMap; concurrent access must be safe.

**Acceptance Criteria:**
- [ ] TypeMap.IsComplete flag is thread-safe (uses proper synchronization)
- [ ] Concurrent requests for the same type do not generate duplicate code
- [ ] Generated IL is cached atomically
- [ ] Type registration is idempotent (same type registered twice is safe)
- [ ] No race conditions when multiple types are registered simultaneously

**Dependencies:** None (concurrency)

**Status:** [GAP] Current code: `TypeMap.IsComplete` is mutable with no synchronization (line ~1850 in Emit.fs). Can cause race condition.

### R11: IL Generation Recursion Limits [GAP]
**Description:** Deeply nested types must not cause stack overflow during IL generation.

**Acceptance Criteria:**
- [ ] Recursion depth limit is enforced during IL generation (e.g., max 100 levels)
- [ ] IL generation fails with clear error if recursion limit exceeded
- [ ] Recursive type definitions are detected and reported
- [ ] Tail-recursive or iterative IL generation is preferred where possible

**Dependencies:** None (robustness)

**Status:** [GAP] Current code: No recursion depth tracking. Recursive schema can cause stack overflow during IL emit.

### R12: Error Reporting in Serialization [GAP]
**Description:** Serialization errors must be reported clearly with context.

**Acceptance Criteria:**
- [ ] Type mismatch during serialization fails with clear error (expected vs. actual type)
- [ ] Missing required element during deserialization fails with element name
- [ ] Invalid value format during deserialization fails with line/column info
- [ ] IO errors (stream closed, out of memory) are propagated with context
- [ ] Errors are not swallowed by fallback logic
- [ ] Error messages are actionable (suggest fix or validation)

**Dependencies:** None (error handling)

**Status:** [GAP] Current code: Uses `failwith` extensively without context. Deserialization errors at line ~1600 lack location information.

### R13: Custom Serialization Hooks [GAP] (Optional)
**Description:** Generated types may provide custom serialization behavior via methods.

**Acceptance Criteria:**
- [ ] Types can implement ISerializable-like interface for custom serialization
- [ ] Custom serialize/deserialize methods are called if present
- [ ] Default IL-based serialization is bypassed for types with custom hooks
- [ ] Custom hooks receive complete serializer context (attachments, default offset, etc.)

**Dependencies:** R1, R2

**Status:** [GAP] Current code: No custom serialization hook mechanism. All serialization is IL-emitted.

## Out of Scope

- SOAP fault serialization (see cavekit-protocol)
- SOAP header serialization (client/service identifiers, see cavekit-protocol)
- Type generation and IL compilation infrastructure (that's part of IL emit setup, not serialization logic)
- Performance optimization (JIT compilation, caching strategies beyond basic TypeMap)
- SOAP encoding (RPC-style) — only document/literal is fully supported
- SOAP version negotiation (SOAP 1.1 only)

## Cross-References

- **cavekit-protocol**: Sends serialized messages via HTTP, deserializes responses
- **cavekit-type-generation**: Generated types are decorated with attributes that guide serialization
- **cavekit-core-types**: Custom attributes (XRoadType, XRoadElement, etc.) are interpreted here

## Source Traceability

**Source files:**
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Emit.fs` (1862 lines) — IL generation for serializers/deserializers

**Related files:**
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Attributes.fs` — Custom attributes read during IL emission
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Choices.fs` — Choice type handling
- `src/FSharp.Data.XRoad/FSharp.Data.XRoad.Protocol.fs` — Uses generated serializers/deserializers

**Typical flow:**
1. User calls generated service operation (client interface, see cavekit-type-generation)
2. cavekit-protocol constructs SOAP request message
3. Serialization delegates from TypeMap are invoked to convert parameters to XML
4. Serialized XML is sent via HTTP (cavekit-protocol)
5. Response is received and deserialization delegates are invoked
6. Response XML is converted back to CLR objects
7. Result is returned to user
