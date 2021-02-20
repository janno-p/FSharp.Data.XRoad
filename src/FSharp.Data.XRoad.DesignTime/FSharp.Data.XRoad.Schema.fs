module internal FSharp.Data.XRoad.Schema

open FSharp.Data.XRoad.Wsdl
open System
open System.Collections.Generic
open System.Globalization
open System.Xml.Linq

/// Extract numerical bound limits from current element.
let readBoundsValue name node =
    match node |> Xml.attr name with
    | Some("unbounded") -> UInt32.MaxValue
    | Some(x) -> UInt32.Parse(x)
    | _ -> 1u

/// General function for reading typed values from attributes with unqualified name.
let readValue fparse name defValue node =
    match node |> Xml.attr (XName.Get(name)) with
    | Some(v) -> fparse(v)
    | _ -> defValue

/// Read minOccurs value from current element, use default value 1 if attribute is missing.
let readMinOccurs: XElement -> uint32 = readBoundsValue (XName.Get("minOccurs"))

/// Read maxOccurs value from current element, use default value 1 if attribute is missing.
let readMaxOccurs: XElement -> uint32 = readBoundsValue (XName.Get("maxOccurs"))

/// Read boolean value from attribute.
let readBoolean name node = readValue Boolean.Parse name false node

/// Read integer value from attribute.
let readInt name node = readValue Int32.Parse name 0 node

/// Read attribute contents as decimal value.
let readDecimal name node = readValue (fun x -> Decimal.Parse(x, CultureInfo.InvariantCulture)) name 0m node

/// Read boolean value which identifies if attribute is nillable.
let readNillable: XElement -> bool = readBoolean "nillable"

/// Helper method for parsing to notify about erroneous schema element definitions.
let notExpectedIn containerName (node: XElement) =
    failwithf "Element %A inside %s element was not expected at the current position!" node.Name containerName

/// Helper method for parsing to notify about schema element definition constructs which are not yet supported.
let notImplementedIn containerName (node: XElement) =
    failwithf "Element %A inside %s element is not implemented yet." node.Name containerName

/// Describes attribute usage.
type AttributeUse =
    | Optional
    | Prohibited
    | Required
    with
        static member FromNode(node) =
            match node |> Xml.attrOrDefault (XName.Get("use")) "optional" with
            | "optional" -> Optional
            | "prohibited" -> Prohibited
            | "required" -> Required
            | x -> failwithf "Invalid attribute use value %s" x

type TypeDefinition<'T> =
    | Definition of 'T
    | Name of XName

/// Schema objects can be defined using qualified name of global definition, referencing another object with
/// `ref` attribute or give object definition in place.
type RefOrTypeDefinition<'T> =
    | Explicit of TypeDefinition<'T>
    | Reference of XName

/// Type schemas `element` node definition.
type ElementSpec =
    { Annotation: Annotation option
      Name: string option
      Namespace: string option
      MinOccurs: uint32
      MaxOccurs: uint32
      IsNillable: bool
      Definition: RefOrTypeDefinition<SchemaTypeDefinition>
      ExpectedContentTypes: string option }
    static member FromNode(node, schema: SchemaNode) =
        { Annotation = None
          Name = None
          Namespace = if node |> Xml.attr (XName.Get("form")) |> Option.map ((=) "qualified") |> Option.defaultValue schema.QualifiedElements then Some(schema.TargetNamespace.NamespaceName) else None
          MinOccurs = readMinOccurs node
          MaxOccurs = readMaxOccurs node
          IsNillable = readNillable node
          Definition = Explicit(Name(XName.Get("anyType", XmlNamespace.Xsd)))
          ExpectedContentTypes = node |> Xml.attr (XName.Get("expectedContentTypes", XmlNamespace.Xmime)) }

/// Schema can give definitions simpleType or complexType; EmptyType is used when type information is not present.
and SchemaTypeDefinition =
    | EmptyDefinition
    | ComplexDefinition of ComplexTypeSpec
    | SimpleDefinition of SimpleTypeSpec

/// Wraps complex type definition.
and ComplexTypeSpec =
    { Annotation: Annotation option
      IsAbstract: bool
      Content: ComplexTypeContent }

/// Simple types can restrict existing simple types or combine existing simple types to list and unions.
and SimpleTypeSpec =
    | Restriction of SimpleTypeRestrictionSpec * Annotation option
    | ListDef
    | Union of UnionSpec

/// Wraps `complexType` node content definition.
and ComplexTypeContent =
    | Empty
    | SimpleContent of SimpleContentSpec
    | ComplexContent of ComplexContentSpec
    | Particle of ComplexTypeContentSpec

/// Describes other simpleType definition to restrict and wraps the definition of restrictions.
and SimpleTypeRestrictionSpec =
    { Base: XName
      SimpleType: SimpleTypeSpec option
      Content: RestrictionContent list }

/// Wraps `union` node definition (types included in union).
and UnionSpec =
    { MemberTypeNames: XName list
      MemberTypes: SimpleTypeSpec list }

/// Complex type `simpleContent` either restricts or extends existing simple types.
and SimpleContentSpec =
    | Restriction of SimpleContentRestrictionSpec
    | Extension of ExtensionSpec

/// Complex type `complexContent` either restricts or extends existing types.
and ComplexContentSpec =
        | Restriction of ComplexContentRestrictionSpec
        | Extension of ExtensionSpec

/// Complex type content defines elements and attributes that are allowed in that type.
and ComplexTypeContentSpec =
    { Content: ComplexTypeParticle option
      Attributes: AttributeSpec list
      AttributeGroups: AttributeGroupSpec list }

/// Various options to restrict simple type definitions.
and RestrictionContent =
    | MinExclusive of decimal
    | MinInclusive of decimal
    | MaxExclusive of decimal
    | MaxInclusive of decimal
    | TotalDigits of int
    | FractionDigits of int
    | Length of int
    | MinLength of int
    | MaxLength of int
    | Enumeration of string
    | WhiteSpace
    | Pattern of string

/// Simple content restriction defines simple type to restrict and restrictions to apply on that type.
and SimpleContentRestrictionSpec =
    { Base: XName
      SimpleType: SimpleTypeSpec option
      Content: RestrictionContent list
      Attributes: AttributeSpec list }

/// Extension identifies type to extend and gives definition for extension.
and ExtensionSpec =
    { Base: XName
      Content: ComplexTypeContentSpec }

/// Complex content restriction identifies type to restrict and gives definition for restrictions.
and ComplexContentRestrictionSpec =
    { Base: XName
      Content: ComplexTypeContentSpec }

/// Complex type can define its content by referencing global group definitions, list of possible elements,
/// alternative combinations of elements, or element sequences.
and ComplexTypeParticle =
    | Group
    | All of AllSpec
    | Choice of ParticleSpec
    | Sequence of ParticleSpec

/// Single attribute definition.
and AttributeSpec =
    { Annotation: Annotation option
      Name: string option
      RefOrType: RefOrTypeDefinition<SimpleTypeSpec>
      Use: AttributeUse
      /// Used for SOAP-encoded array-s.
      ArrayType: (XName * int) option }

/// Elements defined in `all` node can appear in any order.
and AllSpec =
    { MinOccurs: uint32
      MaxOccurs: uint32
      Elements: ElementSpec list }

/// Defines alternatives for current choice or sequence.
and ParticleSpec =
    { Annotation: string option
      MaxOccurs: uint32
      MinOccurs: uint32
      Content: ParticleContent list }

/// Single choice alternative or sequence can contain `any` node to mark acceptance of any element; sub-choice nodes;
/// concrete element definitions; references to predefined element groups; or element sequences.
and ParticleContent =
    | Any
    | Choice of ParticleSpec
    | Element of ElementSpec
    | Group
    | Sequence of ParticleSpec

/// Wrap multiple attribute definitions into predefined group.
and AttributeGroupSpec =
    { Annotation: string
      Attributes: AttributeSpec list
      AttributeGroups: AttributeGroupSpec list
      AllowAny: bool }

/// Documentation info extracted from service descriptions.
and Annotation = { AppInfo: XElement list }

/// Root type to hold definition for entire type schema.
and SchemaNode =
    { QualifiedAttributes: bool
      QualifiedElements: bool
      TargetNamespace: XNamespace
      Attributes: IDictionary<XName,AttributeSpec>
      Elements: IDictionary<XName,ElementSpec>
      Types: IDictionary<XName,SchemaTypeDefinition>
      AttributeGroups: IDictionary<XName,TypeDefinition<AttributeGroupSpec>> }
    /// Merge schema node with another defining same namespace.
    member this.Merge(other: SchemaNode) =
        if this.QualifiedAttributes <> other.QualifiedAttributes then
            failwith "Same namespace has inconsistent values for qualified attributes in different schema files."
        if this.QualifiedElements <> other.QualifiedElements then
            failwith "Same namespace has inconsistent values for qualified elements in different schema files."
        other.Attributes |> Seq.iter this.Attributes.Add
        other.Elements |> Seq.iter this.Elements.Add
        other.Types |> Seq.iter this.Types.Add
        other.AttributeGroups |> Seq.iter this.AttributeGroups.Add
    /// Initializes empty SchemaNode from given `schema` node.
    static member FromNode(node) =
        { QualifiedAttributes = node |> Xml.isQualified (XName.Get("attributeFormDefault"))
          QualifiedElements = node |> Xml.isQualified (XName.Get("elementFormDefault"))
          TargetNamespace = XNamespace.Get(node |> Xml.attrOrDefault (XName.Get("targetNamespace")) "")
          Attributes = Dictionary<_,_>()
          Elements = Dictionary<_,_>()
          Types = Dictionary<_,_>()
          AttributeGroups = Dictionary<_,_>() }

module Parser =
    /// Keeps internal state of parsing for current node.
    type private State =
        | Begin
        | Header
        | Annotation
        | Content
        | Particle
        | Attribute
        | AnyAttribute
        | TypeSpec
        | Other

    /// Extracts documentation from annotation element definition.
    let private parseAnnotation (parentNode: XElement): Annotation option =
        match parentNode.Element(XName.Get("annotation", XmlNamespace.Xsd)) with
        | null -> None
        | node ->
            match node.Elements(XName.Get("appinfo", XmlNamespace.Xsd)) |> List.ofSeq with
            | [] -> None
            | elements -> Some({ AppInfo = elements })

    /// Extracts complexType specification from schema definition.
    let rec private parseComplexType (schema: SchemaNode) (node: XElement): ComplexTypeSpec =
        let parseChildElements() =
            node.Elements()
            |> Seq.fold (fun (state, spec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "simpleContent", (Begin | Annotation) ->
                    Content, Some(SimpleContent(parseSimpleContent schema node))
                | Xsd "complexContent", (Begin | Annotation) ->
                    Content, Some(ComplexContent(parseComplexContent schema node))
                | Xsd "choice", (Begin | Annotation) ->
                    Particle, Some(ComplexTypeContent.Particle({ Content = Some(ComplexTypeParticle.Choice(parseChoice schema node)); Attributes = []; AttributeGroups = [] }))
                | Xsd "group", (Begin | Annotation) ->
                    Particle, node |> notImplementedIn "complexType"
                | Xsd "sequence", (Begin | Annotation) ->
                    Particle, Some(ComplexTypeContent.Particle({ Content = Some(ComplexTypeParticle.Sequence(parseSequence schema node)); Attributes = []; AttributeGroups = [] }))
                | Xsd "all", (Begin | Annotation) ->
                    Particle, Some(ComplexTypeContent.Particle({ Content = Some(All(parseAll schema node)); Attributes = []; AttributeGroups = [] }))
                | Xsd "attribute", (Begin | Annotation | Particle | Attribute) ->
                    let attribute = parseAttribute node
                    let content = match spec with
                                  | Some(ComplexTypeContent.Particle(content)) -> { content with Attributes = content.Attributes @ [attribute] }
                                  | None -> { Content = None; Attributes = [attribute]; AttributeGroups = [] }
                                  | _ -> node |> notExpectedIn "complexType"
                    Attribute, Some(ComplexTypeContent.Particle(content))
                | Xsd "attributeGroup", (Begin | Annotation | Particle | Attribute) ->
                    let attributeGroup = parseAttributeGroup node
                    let content = match spec with
                                  | Some(ComplexTypeContent.Particle(content)) -> { content with AttributeGroups = content.AttributeGroups @ [attributeGroup] }
                                  | None -> { Content = None; Attributes = []; AttributeGroups = [attributeGroup] }
                                  | _ -> node |> notExpectedIn "complexType"
                    Attribute, Some(ComplexTypeContent.Particle(content))
                | Xsd "anyAttribute", (Begin | Annotation | Particle | Attribute) ->
                    Attribute, node |> notImplementedIn "complexType"
                | _ ->
                    node |> notExpectedIn "complexType"
                ) (Begin, None)
                |> snd
                |> Option.defaultValue Empty
        { IsAbstract = node |> readBoolean "abstract"; Content = parseChildElements(); Annotation = parseAnnotation(node) }

    /// Extracts complexType-s simpleContent element specification from schema definition.
    and private parseSimpleContent (schema: SchemaNode) (node: XElement): SimpleContentSpec =
        let content =
            node.Elements()
            |> Seq.fold (fun (state, spec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "restriction", (Begin | Annotation) ->
                    Content, Some(SimpleContentSpec.Restriction(parseSimpleContentRestriction node))
                | Xsd "extension", (Begin | Annotation) ->
                    Content, Some(SimpleContentSpec.Extension(parseExtension schema node))
                | _ -> node |> notExpectedIn "simpleContent"
                ) (Begin, None)
            |> snd
        match content with
        | Some content -> content
        | _ -> failwith "Element simpleContent is expected to contain either restriction or extension element."

    /// Extracts complexType-s complexContent element specification from schema definition.
    and private parseComplexContent (schema: SchemaNode) (node: XElement): ComplexContentSpec =
        let content =
            node.Elements()
            |> Seq.fold (fun (state, spec: ComplexContentSpec option) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "restriction", (Begin | Annotation) ->
                    Content, Some(Restriction(parseComplexContentRestriction schema node))
                | Xsd "extension", (Begin | Annotation) ->
                    Content, Some(Extension(parseExtension schema node))
                | _ -> node |> notExpectedIn "complexContent")
                (Begin, None)
            |> snd
        match content with
        | Some content -> content
        | _ -> failwith "Element complexContent is expected to contain either restriction or extension element."

    /// Extracts choice or sequence element specification from schema definition.
    and private parseParticle (schema: SchemaNode) particleName (node: XElement): ParticleSpec =
        node.Elements()
        |> Seq.fold (fun (state, spec: ParticleSpec) node ->
            match node, state with
            | Xsd "annotation", Begin ->
                Annotation, { spec with Annotation = Some(node.Value) }
            | Xsd "any", _ ->
                Content, { spec with Content = spec.Content @ [Any] }
            | Xsd "choice", _ ->
                Content, { spec with Content = spec.Content @ [Choice(parseChoice schema node)] }
            | Xsd "element", _ ->
                Content, { spec with Content = spec.Content @ [Element(parseElement schema node)] }
            | Xsd "group", _ ->
                Content, node |> notImplementedIn particleName
            | Xsd "sequence", _ ->
                Content, { spec with Content = spec.Content @ [Sequence(parseSequence schema node)] }
            | _ -> node |> notExpectedIn particleName
            ) (Begin, { Annotation = None
                        MaxOccurs = readMaxOccurs node
                        MinOccurs = readMinOccurs node
                        Content = [] })
        |> snd

    and private parseChoice (schema: SchemaNode) node = parseParticle schema "choice" node
    and private parseSequence (schema: SchemaNode) node = parseParticle schema "sequence" node

    /// Extracts `all` element specification from schema definition.
    and private parseAll (schema: SchemaNode) (node: XElement): AllSpec =
        node.Elements()
        |> Seq.fold (fun (state, spec: AllSpec) node ->
            match node, state with
            | Xsd "annotation", Begin ->
                Annotation, spec
            | Xsd "element", (Begin | Annotation | Content) ->
                Content, { spec with Elements = spec.Elements @ [parseElement schema node] }
            | _ -> node |> notExpectedIn "all"
            ) (Begin, { MinOccurs = readMinOccurs node
                        MaxOccurs = readMaxOccurs node
                        Elements = [] })
        |> snd

    /// Extracts `attribute` element specification from schema definition.
    and private parseAttribute (node: XElement): AttributeSpec =
        // Handle SOAP-encoded array definition to get array dimensions.
        let arrayType =
            match node |> Xml.attr (XName.Get("arrayType", XmlNamespace.Wsdl)) with
            | Some value ->
                let ns, name = match value.Split(':') with
                                   | [| local |] -> node.GetDefaultNamespace().NamespaceName, local
                                   | [| prefix; local |] -> node.GetNamespaceOfPrefix(prefix).NamespaceName, local
                                   | _ -> failwithf "Invalid array type: %A" value
                match System.Text.RegularExpressions.Regex.Match(name, @"^(\w+)(\[\])+$") with
                | m when m.Success -> Some(XName.Get(m.Groups.[1].Value, ns), m.Groups.[2].Captures.Count)
                | _ -> failwithf "Invalid array type: %A" value
            | _ -> None
        let attrUse = AttributeUse.FromNode(node)
        // Parse attribute type information.
        match node |> Xml.attr (XName.Get("ref")) with
        | Some refv ->
            match node |> Xml.attr (XName.Get("name")) with
            | Some _ -> failwith "Attribute element name and ref attribute cannot be present at the same time."
            | _ -> { Name = None; RefOrType = Reference(Xml.parseXName node refv); ArrayType = arrayType; Use = attrUse; Annotation = parseAnnotation(node) }
        | _ ->
            let name = node |> Xml.reqAttr (XName.Get("name"))
            match node |> Xml.attr (XName.Get("type")) with
            | Some value -> { Name = Some(name); RefOrType = Explicit(Name(Xml.parseXName node value)); ArrayType = arrayType; Use = attrUse; Annotation = parseAnnotation(node) }
            | _ ->
                node.Elements()
                |> Seq.fold (fun (state, spec) node ->
                    match node, state with
                    | Xsd "annotation", Begin -> Annotation, spec
                    | Xsd "simpleType", (Begin | Annotation) -> TypeSpec, Some(parseSimpleType(node))
                    | _ -> node |> notExpectedIn "attribute"
                    ) (Begin, None)
                |> (fun (_, typ) -> match typ with
                                    | Some(typ) -> { Name = Some(name); RefOrType = Explicit(Definition(typ)); ArrayType = arrayType; Use = attrUse; Annotation = parseAnnotation(node) }
                                    | _ -> failwithf "Attribute element %s type definition is missing." name)

    /// Extracts complexType-s `simpleContent` element specification from schema definition.
    and private parseSimpleContentRestriction (node: XElement): SimpleContentRestrictionSpec =
        node |> notImplementedIn "simpleContent restriction"

    /// Extracts `extension` element specification from schema definition.
    and private parseExtension (schema: SchemaNode) (node: XElement): ExtensionSpec =
        let parseChildElements () =
            node.Elements()
            |> Seq.fold (fun (state, spec: ComplexTypeContentSpec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | (Xsd "group" | Xsd "all" | Xsd "choice"), (Begin | Annotation) ->
                    node |> notImplementedIn "extension"
                | Xsd "sequence", (Begin | Annotation) ->
                    Particle, { spec with Content = Some(ComplexTypeParticle.Sequence(parseSequence schema node)) }
                | (Xsd "attribute" | Xsd "attributeGroup"), (Begin | Annotation | Particle | Attribute) ->
                    Attribute, { spec with Attributes = spec.Attributes @ [parseAttribute node] }
                | Xsd "anyAttribute", (Begin | Annotation | Particle | Attribute) ->
                    node |> notImplementedIn "extension"
                | _ -> node |> notExpectedIn "extension"
                ) (Begin, { Content = None; Attributes = []; AttributeGroups = [] })
            |> snd
        { Base = node |> Xml.reqAttr (XName.Get("base")) |> Xml.parseXName node
          Content = parseChildElements() }

    /// Extracts complexType-s complexContent-s `restriction` element specification from schema definition.
    and private parseComplexContentRestriction (schema: SchemaNode) (node: XElement): ComplexContentRestrictionSpec =
        let parseChildElements () =
            node.Elements()
            |> Seq.fold (fun (state, spec: ComplexTypeContentSpec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "group", (Begin | Annotation)
                | Xsd "all", (Begin | Annotation)
                | Xsd "choice", (Begin | Annotation) ->
                    node |> notImplementedIn "complexContent restriction"
                | Xsd "sequence", (Begin | Annotation) ->
                    Particle,  { spec with Content = Some(ComplexTypeParticle.Sequence(parseSequence schema node)) }
                | Xsd "attribute", (Begin | Annotation | Particle | Attribute) ->
                    Attribute, { spec with Attributes = spec.Attributes @ [parseAttribute node] }
                | Xsd "attributeGroup", (Begin | Annotation | Particle | Attribute) ->
                    Attribute, { spec with AttributeGroups = spec.AttributeGroups @ [parseAttributeGroup node] }
                | Xsd "anyAttribute", (Begin | Annotation | Particle | Attribute) ->
                    node |> notImplementedIn "complexContent restriction"
                | _ ->
                    node |> notExpectedIn "complexContent restriction"
                ) (Begin, { Content = None; Attributes = []; AttributeGroups = [] })
            |> snd
        { Base = node |> Xml.reqAttr (XName.Get("base")) |> Xml.parseXName node
          Content = parseChildElements() }

    /// Extracts `element` element specification from schema definition.
    and private parseElement (schema: SchemaNode) (node: XElement): ElementSpec =
        let parseChildElements () =
            node.Elements()
            |> Seq.fold (fun (state, spec: TypeDefinition<_> option) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "simpleType", (Begin | Annotation) ->
                    TypeSpec, Some(Definition(SimpleDefinition(parseSimpleType node)))
                | Xsd "complexType", (Begin | Annotation) ->
                    TypeSpec, Some(Definition(ComplexDefinition(parseComplexType schema node)))
                | (Xsd "unique" | Xsd "key" | Xsd "keyref"), (Begin | Annotation | TypeSpec | Other) ->
                    node |> notImplementedIn "element"
                | _ -> node |> notExpectedIn "element"
                ) (Begin, None)
            |> snd
        let elementSpec = ElementSpec.FromNode(node, schema)
        match node |> Xml.attr (XName.Get("ref")) with
        | Some refv ->
            match node |> Xml.attr (XName.Get("name")) with
            | Some _ -> failwith "Attribute element name and ref attribute cannot be present at the same time."
            | _ -> { elementSpec with Definition = Reference(Xml.parseXName node refv) }
        | _ ->
            { elementSpec with
                Annotation = parseAnnotation(node)
                Name = Some(node |> Xml.reqAttr (XName.Get("name")))
                Definition =
                    match node |> Xml.attr (XName.Get("type")) with
                    | Some value -> Explicit(Name(Xml.parseXName node value))
                    | _ ->
                        match parseChildElements() with
                        | Some(elements) -> Explicit(elements)
                        | None -> elementSpec.Definition }

    /// Extracts `simpleType` element specification from schema definition.
    and private parseSimpleType (node: XElement): SimpleTypeSpec =
        let annotation = parseAnnotation(node)
        let content =
            node.Elements()
            |> Seq.fold (fun (state, spec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "restriction", (Begin | Annotation) ->
                    Content, Some(SimpleTypeSpec.Restriction(parseSimpleTypeRestriction node, annotation))
                | Xsd "union", (Begin | Annotation) ->
                    Content, Some(Union(parseUnion node))
                | Xsd "list", (Begin | Annotation) ->
                    Content, node |> notImplementedIn "simpleType"
                | _ -> node |> notExpectedIn "simpleType"
                ) (Begin, None)
            |> snd
        match content with
        | Some content -> content
        | _ -> failwith "Element simpleType is expected to contain either restriction, list or union element."

    /// Extracts simpleType-s `restriction` element specification from schema definition.
    and private parseSimpleTypeRestriction (node: XElement): SimpleTypeRestrictionSpec =
        node.Elements()
        |> Seq.fold (fun (state, spec: SimpleTypeRestrictionSpec) node ->
            match node, state with
            | Xsd "annotation", Begin ->
                state, spec
            | Xsd "simpleType", (Begin | Annotation) ->
                TypeSpec, node |> notImplementedIn "simpleType restriction"
            | Xsd "enumeration", (Begin | Annotation | TypeSpec | Content) ->
                let value = node |> Xml.reqAttr(XName.Get("value"))
                Content, { spec with Content = spec.Content @ [Enumeration(value)] }
            | Xsd "fractionDigits", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [FractionDigits(node |> readInt "value")] }
            | Xsd "length", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [Length(node |> readInt "value")] }
            | Xsd "minLength", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [MinLength(node |> readInt "value")] }
            | Xsd "maxLength", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [MaxLength(node |> readInt "value")] }
            | Xsd "minInclusive", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [MinInclusive(node |> readDecimal "value") ] }
            | Xsd "maxInclusive", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [MaxInclusive(node |> readDecimal "value") ] }
            | Xsd "minExclusive", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [MinExclusive(node |> readDecimal "value") ] }
            | Xsd "maxExclusive", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [MaxExclusive(node |> readDecimal "value") ] }
            | Xsd "pattern", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [Pattern(node |> Xml.reqAttr(XName.Get("value")))] }
            | Xsd "totalDigits", (Begin | Annotation | TypeSpec | Content) ->
                Content, { spec with Content = spec.Content @ [TotalDigits(node |> readInt "value")] }
            | Xsd "whiteSpace", (Begin | Annotation | TypeSpec | Content) ->
                Content, node |> notImplementedIn "simpleType restriction"
            | (Xsd "attribute" | Xsd "attributeGroup"), (Begin | Annotation | TypeSpec | Content | Attribute) ->
                Attribute, node |> notImplementedIn "simpleType restriction"
            | Xsd "anyAttribute", (Begin | Annotation | TypeSpec | Content | Attribute) ->
                Other, node |> notImplementedIn "simpleType restriction"
            | _ -> node |> notExpectedIn "simpleType restriction"
            ) (Begin, { Base = node |> Xml.reqAttr (XName.Get("base")) |> Xml.parseXName node
                        SimpleType = None
                        Content = [] })
        |> snd

    /// Extracts simpleType-s `union` element specification from schema definition.
    and private parseUnion (node: XElement): UnionSpec =
        { MemberTypeNames =
            match node |> Xml.attr(XName.Get("memberTypes")) with
            | Some(str) ->
                str.Split(' ')
                |> Array.map (fun x ->
                    match x.Split(':') with
                    | [| name |] -> XName.Get(name)
                    | [| ns; name |] -> XName.Get(name, node.GetNamespaceOfPrefix(ns).NamespaceName)
                    | _ -> failwithf "Invalid member type name %s" x)
                |> List.ofArray
            | None -> []
          MemberTypes =
            node.Elements()
            |> Seq.fold (fun (state, spec) node ->
                match node, state with
                | Xsd "annotation", Begin ->
                    Annotation, spec
                | Xsd "simpleType", (Begin | Annotation) ->
                    Content, spec @ [parseSimpleType(node)]
                | _ -> node |> notExpectedIn "union"
                ) (Begin, [])
            |> snd }

    /// Extracts `attributeGroup` element specification from schema definition.
    and private parseAttributeGroup (node: XElement): AttributeGroupSpec =
        node.Elements()
        |> Seq.fold (fun (state, spec: AttributeGroupSpec) node ->
            match node, state with
            | Xsd "annotation", Begin ->
                Annotation, spec
            | Xsd "attribute", (Begin | Annotation | Attribute) ->
                Attribute, { spec with Attributes = (parseAttribute node)::spec.Attributes }
            | Xsd "attributeGroup", (Begin | Annotation | Attribute) ->
                Attribute, { spec with AttributeGroups = (parseAttributeGroup node)::spec.AttributeGroups }
            | Xsd "anyAttribute", (Begin | Annotation | Attribute | Other) ->
                Other, node |> notImplementedIn "attributeGroup"
            | _ -> node |> notExpectedIn "attributeGroup"
            ) (Begin, { Annotation = ""; Attributes = []; AttributeGroups = []; AllowAny = false })
        |> snd

    /// Parses `schema` node contents and completes schemaNode definition details.
    let internal parseSchemaNode schemaNode (node: XElement) =
        node.Elements()
        |> Seq.fold (fun (state, snode, includes, imports) node ->
            match node, state with
            | Xsd "annotation", _ ->
                state, snode, includes, imports
            | Xsd "include", (Begin | Header) ->
                let schloc = node |> Xml.reqAttr (XName.Get("schemaLocation"))
                Header, snode, (schloc :: includes), imports
            | Xsd "import", (Begin | Header) ->
                let ns = node |> Xml.attrOrDefault (XName.Get("namespace")) ""
                let schloc = node |> Xml.attr (XName.Get("schemaLocation"))
                Header, snode, includes, ((XNamespace.Get(ns), schloc) :: imports)
            | Xsd "redefine", (Begin | Header) ->
                node |> notImplementedIn "schema"
            | Xsd "complexType", _ ->
                let name = node |> Xml.reqAttr (XName.Get("name"))
                let typ = ComplexDefinition(parseComplexType schemaNode node)
                snode.Types.Add(XName.Get(name, snode.TargetNamespace.NamespaceName), typ)
                TypeSpec, snode, includes, imports
            | Xsd "element", _ ->
                let element = parseElement schemaNode node
                match element.Name with
                | None -> failwith "`name` attribute is required if the parent element is the schema element"
                | Some(name) -> snode.Elements.Add(XName.Get(name, snode.TargetNamespace.NamespaceName), element)
                TypeSpec, snode, includes, imports
            | Xsd "simpleType", _ ->
                let name = node |> Xml.reqAttr (XName.Get("name"))
                let typ = SimpleDefinition(parseSimpleType node)
                snode.Types.Add(XName.Get(name, snode.TargetNamespace.NamespaceName), typ)
                TypeSpec, snode, includes, imports
            | Xsd "attribute", _ ->
                let attribute = parseAttribute(node)
                snode.Attributes.Add(XName.Get(attribute.Name.Value, snode.TargetNamespace.NamespaceName), attribute)
                TypeSpec, snode, includes, imports
            | Xsd "attributeGroup", _ ->
                let ag = node |> parseAttributeGroup
                match node |> Xml.attr (XName.Get("name")), node |> Xml.attr (XName.Get("ref")) with
                | Some _, Some _ ->
                    failwithf "Name and ref attributes cannot both be present (%A)" node.Name.LocalName
                | Some(name), None ->
                    snode.AttributeGroups.Add(XName.Get(name, snode.TargetNamespace.NamespaceName), Definition(ag))
                | None, Some(ref) ->
                    let name =
                        match ref.Split(':') with
                        | [| nm |] -> XName.Get(nm, snode.TargetNamespace.NamespaceName)
                        | [| pr; nm |] -> XName.Get(nm, node.GetNamespaceOfPrefix(pr).NamespaceName)
                        | _ -> failwith "wrong ref"
                    snode.AttributeGroups.Add(name, Definition(ag))
                | _ ->
                    node |> notImplementedIn "schema"
                TypeSpec, snode, includes, imports
            | (Xsd "group" | Xsd "notation"), _ -> node |> notImplementedIn "schema"
            | _ -> node |> notExpectedIn "schema"
            ) (Begin, schemaNode, [], [])
        |> (fun (_,a,b,c) -> (a,b,c))

    let fixUri (contextUri: Uri option) path =
        match Uri.TryCreate(path, UriKind.Absolute), contextUri with
        | (true, absUri), _ -> absUri
        | _, Some(uri) -> match Uri.TryCreate(uri, path) with
                          | true, absUri -> absUri
                          | _ -> failwithf "Unable to detect uri `%s` in the context of `%A`." path uri
        | _ -> failwithf "Could not resolve uri `%s`." path

    /// Collect type definitions of imported schemas.
    let rec private collectImportedSchemas schemaUri schemaLookup (documentSchemas: Map<_,XElement>) (imports: (XNamespace * string option) list) =
        imports
        |> List.filter
            (fun (ns, _) -> XmlNamespace.predefined |> List.exists ((=) ns.NamespaceName) |> not)
        |> List.iter
            (fun (ns, uri) ->
                match uri, documentSchemas.TryFind(ns.NamespaceName) with
                | None, Some _ -> ()
                | _ ->
                    let path = (uri |> Option.defaultValue ns.NamespaceName) |> fixUri schemaUri
                    let schemaNode =
                        let doc = Http.getXDocument path
                        doc.Element (XName.Get("schema", XmlNamespace.Xsd))
                        |> findSchemaNode path schemaLookup documentSchemas
                    if schemaNode.TargetNamespace <> ns then
                        failwithf "Imported type schema targetNamespace `%s` does not match with expected namespace value `%s` on import element." schemaNode.TargetNamespace.NamespaceName ns.NamespaceName)

    /// Collect type definitions from included schemas.
    and private collectIncludedSchemas targetNamespace schemaUri schemaLookup documentSchemas includes =
        includes
        |> List.iter
            (fun uri ->
                let path =
                    uri |> fixUri schemaUri
                let schemaNode =
                    let doc = Http.getXDocument path
                    doc.Element(XName.Get("schema", XmlNamespace.Xsd))
                    |> findSchemaNode path schemaLookup documentSchemas
                if schemaNode.TargetNamespace <> targetNamespace then
                    failwith "Included type schema should define same target namespace as the schema including it.")

    /// Parses all definitions in given schema node.
    and private findSchemaNode (schemaUri: Uri) (schemaLookup: Dictionary<(string * string),SchemaNode>) documentSchemas node =
        let schemaNode = SchemaNode.FromNode(node)
        // Use previously parsed schema if present.
        match schemaLookup.TryGetValue((schemaNode.TargetNamespace.NamespaceName, schemaUri.ToString())) with
        | false, _ ->
            let schema, includes, imports = node |> parseSchemaNode schemaNode
            schemaLookup.Add((schemaNode.TargetNamespace.NamespaceName, schemaUri.ToString()), schema)
            imports |> collectImportedSchemas (Some schemaUri) schemaLookup documentSchemas
            includes |> collectIncludedSchemas schema.TargetNamespace (Some schemaUri) schemaLookup documentSchemas
            schema
        | true, schema -> schema

    let private getDocumentSchemas (typesNode: XElement) =
        typesNode.Elements(XName.Get("schema", XmlNamespace.Xsd))
        |> Seq.map (fun schemaNode ->
            match schemaNode.Attribute(XName.Get("targetNamespace")) with
            | null -> (None, schemaNode)
            | attr -> (Some(attr.Value), schemaNode))
        |> Seq.toList

    /// Parses all type schemas defined and referenced in current WSDL document.
    let parseSchema path (definitions: XElement) =
        match definitions.Element(XName.Get("types", XmlNamespace.Wsdl)) with
        | null -> Map.empty
        | typesNode ->
            let schemaLookup = Dictionary<_,_>()
            let uri = fixUri None path
            let documentSchemas = getDocumentSchemas typesNode
            let documentSchemaLookup =
                documentSchemas
                |> List.choose (fun (ns, schemaNode) -> ns |> Option.map (fun ns -> (ns, schemaNode)))
                |> Map.ofList
            documentSchemas
            |> List.iter
                (fun (ns, node) ->
                    match ns with
                    | None ->
                        let schemaNode = SchemaNode.FromNode(node)
                        let _, _, imports = parseSchemaNode schemaNode node
                        imports |> collectImportedSchemas (Some uri) schemaLookup documentSchemaLookup
                    | Some _ -> findSchemaNode uri schemaLookup documentSchemaLookup node |> ignore)
            schemaLookup
            |> Seq.fold (fun (mergedSchemas: Dictionary<string,SchemaNode>) kvp ->
                match mergedSchemas.TryGetValue (fst kvp.Key) with
                | true, existingSchema -> existingSchema.Merge(kvp.Value)
                | _ -> mergedSchemas.Add(fst kvp.Key, kvp.Value)
                mergedSchemas) (Dictionary<_,_>())
            |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq

/// Combines operations and types documented in producer definitions.
type internal ProducerDescription =
    { LanguageCode: string
      TypeSchemas: Map<string, SchemaNode>
      Services: Service list }

    /// Load producer definition from given uri location.
    static member Load(uri: Uri, languageCode, operationFilter) =
        let document = Http.getXDocument uri
        match document.Element(XName.Get("definitions", XmlNamespace.Wsdl)) with
        | null -> failwithf "Uri `%A` refers to invalid WSDL document (`definitions` element not found)." uri
        | definitions ->
            { LanguageCode = languageCode
              Services = definitions |> parseServices languageCode operationFilter
              TypeSchemas = definitions |> Parser.parseSchema (uri.ToString()) }

    /// Load producer definition from given XML document.
    static member Load(document: XDocument, languageCode, operationFilter) =
        match document.Element(XName.Get("definitions", XmlNamespace.Wsdl)) with
        | null -> failwith "Invalid WSDL document (`definitions` element not found)."
        | definitions ->
            let uri = definitions.Attribute(XName.Get("targetNamespace")).Value
            { LanguageCode = languageCode
              Services = definitions |> parseServices languageCode operationFilter
              TypeSchemas = definitions |> Parser.parseSchema uri }

[<AutoOpen>]
module internal Patterns =
    type ArrayType =
    | Regular of ElementSpec
    | SoapEncArray of ElementSpec

    /// Active pattern which checks type definition against collection characteristics.
    /// Returns match if given type should be treated as CollectionType.
    let (|ArrayContent|_|) (schemaType: SchemaTypeDefinition) =
        // SOAP-encoded array-s use special attribute for array type definition.
        let (|ArrayType|_|) (attributes: AttributeSpec list) =
            attributes |> List.tryFind (fun a -> a.Name = Some("arrayType") || a.RefOrType = Reference(XName.Get("arrayType", XmlNamespace.SoapEnc)))
        // Extracts information about array item type.
        let getArrayItemElement contentParticle =
            match contentParticle with
            | Some(All(all)) ->
                if all.MaxOccurs > 1u then failwith "Not implemented: array of anonymous all types."
                elif all.MaxOccurs < 1u then None
                else match all.Elements with
                     | [ single ] when single.MaxOccurs > 1u -> Some(single)
                     | _ -> None
            | Some(ComplexTypeParticle.Choice(choice)) ->
                if choice.MaxOccurs > 1u then failwith "Not implemented: array of anonymous choice types."
                elif choice.MaxOccurs < 1u then None
                elif (choice.Content
                      |> List.fold (fun state ch ->
                            match state, ch with
                            | true, Element(e) when e.MaxOccurs < 2u -> true
                            | true, Sequence(s) when s.MaxOccurs < 2u -> true
                            | _ -> false) true)
                     then None
                else failwith "Not implemented: array of varying choice types."
            | Some(ComplexTypeParticle.Sequence(sequence)) ->
                if sequence.MaxOccurs > 1u then
                    match sequence.Content with
                    | [] -> None
                    | [ Element(single) ] -> Some(single)
                    | _ -> failwith "Not implemented: array of anonymous sequence types."
                elif sequence.MaxOccurs < 1u then None
                else match sequence.Content with
                     | [ Element(single) ] when single.MaxOccurs > 1u -> Some(single)
                     | _ -> None
            | Some(ComplexTypeParticle.Group) -> failwith "group not implemented."
            | None -> None
        // Test type definitions for collection characteristics.
        match schemaType with
        | ComplexDefinition(spec) ->
            match spec.Content with
            // SOAP-encoded array-s inherit soapenc:Array type.
            | ComplexContent(Restriction(rstr)) when rstr.Base.LocalName = "Array" && rstr.Base.NamespaceName = XmlNamespace.SoapEnc ->
                match rstr.Content.Attributes with
                | ArrayType(attrSpec) ->
                    match attrSpec.ArrayType with
                    | Some(_, rank) when rank <> 1 ->
                        failwith "Multidimensional SOAP encoding arrays are not supported."
                    | Some(typeName, _) ->
                        match getArrayItemElement(rstr.Content.Content) with
                        | Some(element) -> Some({ element with Definition = Explicit(Name(typeName)) })
                        | None -> Some({ Name = Some("item"); Namespace = None; MinOccurs = 0u; MaxOccurs = UInt32.MaxValue; IsNillable = true; Definition = Explicit(Name(typeName)); Annotation = None; ExpectedContentTypes = None })
                        |> Option.map SoapEncArray
                    | None ->
                        failwith "Array underlying type specification is missing."
                | _ ->
                    match getArrayItemElement(rstr.Content.Content) with
                    | Some _ as element -> element |> Option.map Regular
                    | None -> failwith "Unsupported SOAP encoding array definition."
            // Multiplicity my constrain to using collection type.
            | Particle(content) -> getArrayItemElement(content.Content) |> Option.map Regular
            | _ -> None
        | EmptyDefinition
        | SimpleDefinition _ -> None

