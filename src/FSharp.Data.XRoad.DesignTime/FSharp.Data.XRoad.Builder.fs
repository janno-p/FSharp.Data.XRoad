module internal FSharp.Data.XRoad.Builder

open FSharp.Data.XRoad.Attributes
open FSharp.Data.XRoad.Choices
open FSharp.Data.XRoad.Schema
open FSharp.Data.XRoad.Wsdl
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open System
open System.Collections.Generic
open System.Reflection
open System.Xml.Linq

/// Type abstraction for code generator.
type RuntimeType =
    /// Represents anonymous type (xs:any definition).
    | AnyType
    /// Represents missing type.
    | UnitType
    /// Simple types that are presented with system runtime types.
    | PrimitiveType of Type * TypeHint
    /// Types that are provided by generated assembly.
    | ProvidedType of ProvidedTypeDefinition
    /// Types that represent collection or array of runtime type.
    | CollectionType of RuntimeType * string * SchemaTypeDefinition option
    /// Binary content types are handled separately.
    | ContentType of TypeHint
with
    member this.TypeHint
        with get() =
            match this with
            | ContentType(TypeHint.None)
            | PrimitiveType(_, TypeHint.None) -> None
            | ContentType(thv)
            | PrimitiveType(_, thv) -> Some(thv)
            | _ -> None

module internal String =
    open System.Globalization
    open System.Text

    let inline isNullOrEmpty x = String.IsNullOrEmpty(x)

    // http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-334.pdf

    let isLetterCharacter (ch: char) =
        match CharUnicodeInfo.GetUnicodeCategory(ch) with
        | UnicodeCategory.UppercaseLetter
        | UnicodeCategory.LowercaseLetter
        | UnicodeCategory.TitlecaseLetter
        | UnicodeCategory.ModifierLetter
        | UnicodeCategory.OtherLetter
        | UnicodeCategory.LetterNumber -> true
        | _ -> false

    let isCombiningCharacter (ch: char) =
        match CharUnicodeInfo.GetUnicodeCategory(ch) with
        | UnicodeCategory.NonSpacingMark
        | UnicodeCategory.SpacingCombiningMark -> true
        | _ -> false

    let inline private isDecimalDigitCharacter (ch: char) = CharUnicodeInfo.GetUnicodeCategory(ch) = UnicodeCategory.DecimalDigitNumber
    let inline private isConnectingCharacter (ch: char) = CharUnicodeInfo.GetUnicodeCategory(ch) = UnicodeCategory.ConnectorPunctuation
    let inline private isFormattingCharacter (ch: char) = CharUnicodeInfo.GetUnicodeCategory(ch) = UnicodeCategory.Format
    let inline private isUnderscoreCharacter (ch: char) = ch = '_'
    let inline private isIdentifierStartCharacter (ch: char) = isLetterCharacter ch || isUnderscoreCharacter ch

    let private isIdentifierPartCharacter (ch: char) =
        isLetterCharacter ch || isDecimalDigitCharacter ch || isConnectingCharacter ch || isCombiningCharacter ch || isFormattingCharacter ch

    let private isValidIdentifier (name: string) =
        if name |> isNullOrEmpty then false else
        if isIdentifierStartCharacter name.[0] |> not then false else
        Array.TrueForAll(name.ToCharArray() |> Array.skip 1, Predicate(isIdentifierPartCharacter))

    /// Joins sequence of elements with given separator to string.
    let inline join (sep: string) (arr: seq<'T>) = String.Join(sep, arr)

    /// Converts given XML namespace to class name.
    let xmlNamespaceToClassName (this: string) =
        // Remove `http://` prefix from namespace if present.
        let str =
            match this.StartsWith("http://") with
            | true -> this.Substring(7)
            | _ -> this
        // Remove special symbols from class name.
        let className =
            str.Split('/')
            |> Array.map (fun p ->
                p.Split('.')
                |> Array.map (fun x -> CultureInfo.InvariantCulture.TextInfo.ToTitleCase(x.ToLower()).Replace("-", ""))
                |> join "")
            |> join "_"
        // Check validity of generated class name.
        if not (isValidIdentifier className) then failwithf "invalid name %s" className
        className

    let asValidIdentifierName (this: string) =
        let propertyName = StringBuilder()
        if not (isIdentifierStartCharacter this.[0]) then propertyName.Append("_") |> ignore
        this.ToCharArray() |> Array.iter (fun c ->
            if isIdentifierPartCharacter c then propertyName.Append(c) |> ignore
            elif propertyName.[propertyName.Length - 1] <> '_' then propertyName.Append('_') |> ignore
            else ())
        let fixedName = propertyName.ToString()
        if not (isValidIdentifier fixedName) then failwithf "Invalid property name `%s`." fixedName
        fixedName

    let capitalize (this: string) =
        match this with
        | null | "" -> this
        | _ -> sprintf "%c%s" (Char.ToUpper(this.[0])) (this.Substring(1))

[<RequireQualifiedAccess>]
module internal CustomAttribute =
    open System.Diagnostics
    open System.Xml.Schema
    open System.Xml.Serialization

    let private unqualifiedFormArgument (typ: Type) =
        CustomAttributeNamedArgument(typ.GetProperty("Form"), CustomAttributeTypedArgument(typeof<XmlSchemaForm>, XmlSchemaForm.Unqualified))

    let optional () =
        let typ = typeof<Runtime.InteropServices.OptionalAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([||])
            member __.ConstructorArguments = upcast [||]
            member __.NamedArguments = upcast [||]
        }

    let xmlIgnore () =
        let typ = typeof<XmlIgnoreAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([||])
            member __.ConstructorArguments = upcast [||]
            member __.NamedArguments = upcast [||]
        }

    let xmlAnyElement () =
        let typ = typeof<XmlAnyElementAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([||])
            member __.ConstructorArguments = upcast [||]
            member __.NamedArguments = upcast [||]
        }

    let xmlAttribute () =
        let typ = typeof<XmlAttributeAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([||])
            member __.ConstructorArguments = upcast [||]
            member __.NamedArguments = upcast [| unqualifiedFormArgument typ |]
        }

    let debuggerBrowsable () =
        let typ = typeof<DebuggerBrowsableAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([| typeof<DebuggerBrowsableState> |])
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<DebuggerBrowsableState>, DebuggerBrowsableState.Never) |]
            member __.NamedArguments = upcast [||]
        }

    let xrdType (typeName: XName) (layout: LayoutKind) =
        let typ = typeof<XRoadTypeAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([| typeof<string>; typeof<LayoutKind> |])
            member __.ConstructorArguments = upcast [|
                CustomAttributeTypedArgument(typeof<string>, typeName.LocalName)
                CustomAttributeTypedArgument(typeof<LayoutKind>, box layout)
            |]
            member __.NamedArguments = upcast [|
                CustomAttributeNamedArgument(typ.GetProperty("Namespace"), CustomAttributeTypedArgument(typeof<string>, typeName.NamespaceName))
            |]
        }

    let xrdAnonymousType (layout: LayoutKind) =
        let typ = typeof<XRoadTypeAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([| typeof<LayoutKind> |])
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<LayoutKind>, box layout) |]
            member __.NamedArguments = upcast [| CustomAttributeNamedArgument(typ.GetProperty("IsAnonymous"), CustomAttributeTypedArgument(typeof<bool>, true)) |]
        }

    let xrdElement idx name ``namespace`` isNullable mergeContent typeHint =
        let typ = typeof<XRoadElementAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([| typeof<int>; typeof<string> |])
            member __.ConstructorArguments = upcast [|
                CustomAttributeTypedArgument(typeof<int>, box (idx |> Option.defaultValue -1))
                CustomAttributeTypedArgument(typeof<string>, name |> Option.defaultValue "")
            |]
            member __.NamedArguments = upcast [|
                match ``namespace`` with
                | Some(v) ->
                    yield CustomAttributeNamedArgument(typ.GetProperty("Namespace"), CustomAttributeTypedArgument(typeof<string>, v))
                | _ -> ()

                if isNullable then
                    yield CustomAttributeNamedArgument(typ.GetProperty("IsNullable"), CustomAttributeTypedArgument(typeof<bool>, true))

                if mergeContent then
                    yield CustomAttributeNamedArgument(typ.GetProperty("MergeContent"), CustomAttributeTypedArgument(typeof<bool>, true))

                match typeHint with
                | Some(TypeHint.None) | None -> ()
                | Some(thv) ->
                    yield CustomAttributeNamedArgument(typ.GetProperty("TypeHint"), CustomAttributeTypedArgument(typeof<TypeHint>, thv))
            |]
        }

    let xrdCollection idx itemName itemNamespace itemIsNullable mergeContent =
        let typ = typeof<XRoadCollectionAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([| typeof<int>; typeof<string> |])
            member __.ConstructorArguments = upcast [|
                CustomAttributeTypedArgument(typeof<int>, box (idx |> Option.defaultValue -1))
                CustomAttributeTypedArgument(typeof<string>, itemName |> Option.defaultValue "")
            |]
            member __.NamedArguments = upcast [|
                match itemNamespace with
                | Some(v) ->
                    yield CustomAttributeNamedArgument(typ.GetProperty("ItemNamespace"), CustomAttributeTypedArgument(typeof<string>, v))
                | _ -> ()

                if itemIsNullable then
                    yield CustomAttributeNamedArgument(typ.GetProperty("ItemIsNullable"), CustomAttributeTypedArgument(typeof<bool>, true))

                if mergeContent then
                    yield CustomAttributeNamedArgument(typ.GetProperty("MergeContent"), CustomAttributeTypedArgument(typeof<bool>, true))
            |]
        }

    let xrdOperation name (version: string option) =
        let typ = typeof<XRoadOperationAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([| typeof<string>; typeof<string> |])
            member __.ConstructorArguments = upcast [|
                CustomAttributeTypedArgument(typeof<string>, name)
                CustomAttributeTypedArgument(typeof<string>, version |> Option.defaultValue null)
            |]
            member __.NamedArguments = upcast [|
                CustomAttributeNamedArgument(typ.GetProperty("ProtocolVersion"), CustomAttributeTypedArgument(typeof<string>, "4.0"))
            |]
        }

    let xrdRequiredHeaders ns hdrs =
        let typ = typeof<XRoadRequiredHeadersAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([| typeof<string>; typeof<string[]> |])
            member __.ConstructorArguments = upcast [|
                CustomAttributeTypedArgument(typeof<string>, ns)
                CustomAttributeTypedArgument(typeof<string[]>, hdrs |> List.toArray)
            |]
            member __.NamedArguments = upcast [||]
        }

    let xrdRequest name ns isEncoded isMultipart =
        let typ = typeof<XRoadRequestAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([| typeof<string>; typeof<string> |])
            member __.ConstructorArguments = upcast [|
                CustomAttributeTypedArgument(typeof<string>, name)
                CustomAttributeTypedArgument(typeof<string>, ns)
            |]
            member __.NamedArguments = upcast [|
                if isEncoded then
                    yield CustomAttributeNamedArgument(typ.GetProperty("Encoded"), CustomAttributeTypedArgument(typeof<bool>, true))

                if isMultipart then
                    yield CustomAttributeNamedArgument(typ.GetProperty("Multipart"), CustomAttributeTypedArgument(typeof<bool>, true))
            |]
        }

    let xrdResponse name ns isEncoded isMultipart (returnType: Type option) =
        let typ = typeof<XRoadResponseAttribute>
        { new CustomAttributeData () with
            member __.Constructor = typ.GetConstructor([| typeof<string>; typeof<string> |])
            member __.ConstructorArguments = upcast [|
                CustomAttributeTypedArgument(typeof<string>, name)
                CustomAttributeTypedArgument(typeof<string>, ns)
            |]
            member __.NamedArguments = upcast [|
                if isEncoded then
                    yield CustomAttributeNamedArgument(typ.GetProperty("Encoded"), CustomAttributeTypedArgument(typeof<bool>, true))

                if isMultipart then
                    yield CustomAttributeNamedArgument(typ.GetProperty("Multipart"), CustomAttributeTypedArgument(typeof<bool>, true))

                match returnType with
                | Some(ty) ->
                    yield CustomAttributeNamedArgument(typ.GetProperty("ReturnType"), CustomAttributeTypedArgument(typeof<Type>, ty))
                | None -> ()
            |]
        }

[<AutoOpen>]
module internal Helpers =
    open System.Text.RegularExpressions

    let rec cliType (runtimeType: RuntimeType) =
        match runtimeType with
        | AnyType -> typeof<XElement[]>
        | PrimitiveType(typ, _) -> typ
        | ProvidedType(providedTy) -> upcast providedTy
        | CollectionType(typ,_,_) -> (cliType typ).MakeArrayType()
        | ContentType(_) -> typeof<BinaryContent>
        | UnitType -> typeof<Void>

    let (|ProducerName|_|) ns =
        match Regex.Match(ns, @"^https?://((?<producer>\w+)\.x-road\.eu(/(?<path>.+)?)?)$") with
        | m when m.Success ->
            let suffix = if m.Groups.["path"].Success then m.Groups.["path"].Value |> String.xmlNamespaceToClassName |> sprintf "_%s" else ""
            Some(sprintf "%s%s" (m.Groups.["producer"].Value |> String.capitalize) suffix)
        | _ -> None

/// Describes single property for type declaration.
type PropertyDefinition =
    { /// Name of the property.
      Name: string
      /// Qualified namespace of the propertys XML element.
      QualifiedNamespace: string option
      /// Runtime type to use on property.
      Type: RuntimeType
      /// Does property accept nil values?
      IsNillable: bool
      /// Can array items be nil values?
      IsItemNillable: bool option
      /// Can property value be unspecified in resulting SOAP message.
      IsOptional: bool
      /// Does array type property specify wrapper element around items?
      IsWrappedArray: bool option
      // Attribute type:
      IsAttribute: bool
      IsIgnored: bool
      // Documentation tooltips
      Documentation: string option }
    /// Initializes default property with name and optional value.
    static member Create(name, qualifiedNamespace, isOptional, doc) =
        { Type = UnitType
          IsNillable = false
          IsItemNillable = None
          IsOptional = isOptional
          IsWrappedArray = None
          Name = name
          QualifiedNamespace = qualifiedNamespace
          IsAttribute = false
          IsIgnored = false
          Documentation = doc }

/// Context keeps track of already generated types for provided types and namespaces
/// to simplify reuse and resolve mutual dependencies between types.
type internal TypeBuilderContext =
    { /// Provided types generated from type schema definitions.
      CachedTypes: Dictionary<SchemaName, RuntimeType>
      /// Provided types generated to group types from same namespace.
      CachedNamespaces: Dictionary<XNamespace, ProvidedTypeDefinition>
      /// Schema level attribute definition lookup.
      Attributes: Map<string,AttributeSpec>
      /// Schema level element definition lookup.
      Elements: Map<string,ElementSpec>
      /// Schema level type definition lookup.
      Types: Map<string,SchemaTypeDefinition>
      /// Language code preferred for code comments.
      LanguageCode: string }
    with
        /// Find generated type that corresponds to given namespace name.
        /// If type exists, the existing instance is used; otherwise new type is generated.
        member this.GetOrCreateNamespace(nsname: XNamespace) =
            match this.CachedNamespaces.TryGetValue(nsname) with
            | false, _ ->
                let producerName =
                    match nsname.NamespaceName with
                    | ProducerName(producerName) -> producerName
                    | uri -> uri |> String.xmlNamespaceToClassName

                let typ = ProvidedTypeDefinition(producerName, Some typeof<obj>, isErased=false)

                let namespaceField = ProvidedField.Literal("__TargetNamespace__", typeof<string>, nsname.NamespaceName)
                typ.AddMember(namespaceField)

                this.CachedNamespaces.Add(nsname, typ)
                typ
            | true, typ -> typ

        /// Get runtime type from cached types if exists; otherwise create the type.
        member this.GetOrCreateType(name: SchemaName) =
            match this.CachedTypes.TryGetValue(name) with
            | true, info -> info
            | _ -> let info = this.CreateType(name)
                   this.CachedTypes.Add(name, info)
                   info

        /// Get runtime type from cached types if exists.
        member this.GetRuntimeType(name: SchemaName) =
            let resolvedName =
                match name with
                | SchemaElement(xname) ->
                    match this.GetElementSpec(xname) with
                    | ({ Definition = Explicit(Name(typeName)) } : ElementSpec) -> SchemaType(typeName)
                    | _ -> name
                | _ -> name
            match this.CachedTypes.TryGetValue(resolvedName) with
            | true, typeInfo -> typeInfo
            | _ -> match resolvedName.XName with
                   | BinaryType(thv) -> ContentType(thv)
                   | SystemType(args) -> PrimitiveType(args)
                   | _ -> failwithf "Invalid type name `%A`: type not found in cache." resolvedName

        /// Generates new RuntimeType instance depending on given type:
        /// xsd:base64Binary and xsd:hexBinary types represent ContentType.
        /// Types that are mapped to system types represent PrimitiveType value.
        /// Types that have multiplicity larger than 1 are defined as CollectionTypes.
        /// Other types will define separate ProvidedType in generated assembly.
        member private this.CreateType(name: SchemaName) =
            match name.XName with
            | BinaryType(thv) -> ContentType(thv)
            | SystemType(args) -> PrimitiveType(args)
            | _ ->
                let nstyp = this.GetOrCreateNamespace(name.XName.Namespace)
                let schemaType =
                    match name with
                    | SchemaElement(xn) ->
                        this.GetElementSpec(xn)
                        |> this.DereferenceElementSpec
                        |> snd
                        |> this.GetSchemaTypeDefinition
                    | SchemaType(xn) -> this.GetSchemaType(xn)
                match schemaType with
                | ArrayContent element ->
                    match this.DereferenceElementSpec(element) with
                    | dspec, Name(xn) ->
                        let itemName = dspec.Name |> Option.get
                        CollectionType(this.GetOrCreateType(SchemaType(xn)), itemName, None)
                    | dspec, Definition(def) ->
                        let itemName = dspec.Name |> Option.get
                        let suffix = itemName |> String.asValidIdentifierName |> String.capitalize
                        let typ = ProvidedTypeDefinition((name.XName.LocalName |> String.asValidIdentifierName) + suffix, Some typeof<obj>, isErased=false)
                        typ.AddCustomAttribute(CustomAttribute.xrdAnonymousType LayoutKind.Sequence)
                        nstyp.AddMember(typ)
                        CollectionType(ProvidedType(typ), itemName, Some(def))
                | _ ->
                    let attr =
                        match name with
                        | SchemaElement(_) -> CustomAttribute.xrdAnonymousType LayoutKind.Sequence
                        | SchemaType(_) -> CustomAttribute.xrdType name.XName LayoutKind.Sequence
                    let typ = ProvidedTypeDefinition(name.XName.LocalName |> String.asValidIdentifierName, Some typeof<obj>, isErased=false)
                    typ.AddCustomAttribute(attr)
                    nstyp.AddMember(typ)
                    ProvidedType(typ)

        /// Finds element specification from schema-level element lookup.
        member this.GetElementSpec(name: XName) =
            match this.Elements.TryFind(name.ToString()) with
            | Some(elementSpec) -> elementSpec
            | None -> failwithf "Invalid reference: global element %A was not found in current context." name

        /// Finds element specification from schema-level type lookup.
        member this.GetSchemaType(name: XName) =
            match this.Types.TryFind(name.ToString()) with
            | Some(schemaType) -> schemaType
            | None -> failwithf "Invalid reference: global type `%A` was not found in current context." name

        /// Resolves real type definition from lookup by following the XML schema references if present.
        /// Returns value of type definitions which actually contains definition, not references other definition.
        member this.GetSchemaTypeDefinition typeDefinition =
            let rec findSchemaTypeDefinition typeDefinition =
                match typeDefinition with
                | Definition(spec) -> spec
                | Name(xn) -> match this.Types.TryFind(xn.ToString()) with
                              | Some(schemaType) -> schemaType
                              | None -> failwithf "Missing referenced schema type `%A`." xn
            findSchemaTypeDefinition typeDefinition

        /// Resolves real atrribute definition from lookup by following the XML schema references if present.
        /// Returns value of attribute definitions which actually contains definition, not references other definition.
        member this.GetAttributeDefinition(spec) =
            let rec findAttributeDefinition (spec: AttributeSpec) =
                match spec.RefOrType with
                | Explicit(typeDefinition) ->
                    match spec.Name with
                    | Some(name) -> name, typeDefinition
                    | None -> failwithf "Attribute has no name."
                | Reference(ref) ->
                    match this.Attributes.TryFind(ref.ToString()) with
                    | Some(spec) -> findAttributeDefinition(spec)
                    | None ->
                        match ref with
                        | XmlName "lang" -> "lang", Name(XName.Get("string", XmlNamespace.Xsd))
                        | _ -> failwithf "Missing referenced attribute %A." ref
            findAttributeDefinition(spec)

        /// Resolves real element definition from lookup by following the XML schema references if present.
        /// Returns value of element definitions which actually contains definition, not references other definition.
        member this.DereferenceElementSpec(spec): ElementSpec * TypeDefinition<SchemaTypeDefinition> =
            let rec findElementDefinition (spec: ElementSpec) =
                match spec.Definition with
                | Explicit(typeDefinition) ->
                    match spec.Name with
                    | Some(_) -> spec, typeDefinition
                    | None -> failwithf "Attribute has no name."
                | Reference(ref) ->
                    match this.Elements.TryFind(ref.ToString()) with
                    | Some(spec) -> findElementDefinition(spec)
                    | None -> failwithf "Missing referenced attribute %A." ref
            findElementDefinition(spec)

let private initCache (selector: SchemaNode -> IDictionary<XName, _>) (schema: ProducerDescription) =
    schema.TypeSchemas
    |> Map.toSeq
    |> Seq.collect (snd >> selector >> Seq.map (fun x -> x.Key.ToString(), x.Value))
    |> Map.ofSeq

let fixContentType useXop rtyp =
    match rtyp with
    | ContentType(TypeHint.None) when useXop -> ContentType(TypeHint.Xop)
    | rtyp -> rtyp

/// Create definition of property that accepts any element not defined in schema.
let private buildAnyProperty () =
    { PropertyDefinition.Create("AnyElements", None, false, None) with Type = AnyType }

let private annotationToText (context: TypeBuilderContext) (annotation: Annotation option) =
    annotation
    |> Option.bind (fun annotation ->
        annotation.AppInfo
        |> List.collect (fun e -> e.Elements(XName.Get("title", XmlNamespace.XRoad)) |> List.ofSeq)
        |> List.fold (fun doc el ->
            let lang = el |> Xml.attrOrDefault (XName.Get("lang", XmlNamespace.Xml)) "et"
            (lang, el.Value)::doc) []
        |> List.tryFind (fst >> ((=) context.LanguageCode))
        |> Option.map snd)

let nameGenerator name =
    let num = ref 0 in (fun () -> num := !num + 1; sprintf "%s%d" name !num)

/// Add property to given type with backing field.
/// For optional members, extra field is added to notify if property was assigned or not.
let addProperty (name : string, ty: Type, isOptional) (owner: ProvidedTypeDefinition) =
    let name = name |> String.asValidIdentifierName
    let ty = if isOptional then ProvidedTypeBuilder.MakeGenericType(typedefof<Optional.Option<_>>, [ty]) else ty

    let f = ProvidedField(sprintf "%s__backing" name, ty)
    f.AddCustomAttribute(CustomAttribute.debuggerBrowsable())
    owner.AddMember(f)

    let propName = if name = owner.Name then sprintf "%s_" name else name
    let p = ProvidedProperty(propName, ty, getterCode=(fun args -> Expr.FieldGet(Expr.Coerce(args.[0], owner), f)), setterCode=(fun args -> Expr.FieldSet(Expr.Coerce(args.[0], owner), f, args.[1])))
    owner.AddMember(p)

    p

let addContentProperty (name: string, ty: RuntimeType, predefinedValues) (owner: ProvidedTypeDefinition) =
    let name = name |> String.asValidIdentifierName
    let systemType = cliType ty

    let f = ProvidedField(name + "__backing", systemType)
    owner.AddMember(f)

    let p = ProvidedProperty(name, systemType, getterCode = (fun args -> Expr.FieldGet(Expr.Coerce(args.[0], owner), f)))
    p.AddCustomAttribute(CustomAttribute.xrdElement None None None false true ty.TypeHint)
    owner.AddMember(p)

    let methodAttributes = (if predefinedValues then MethodAttributes.Private else MethodAttributes.Public) ||| MethodAttributes.RTSpecialName
    let parameters = [ ProvidedParameter("value", systemType) ]
    let ctor = ProvidedConstructor(parameters, methodAttributes, (fun args -> Expr.FieldSet(Expr.Coerce(args.[0], owner), f, args.[1])))
    owner.AddMember(ctor)

    ctor

let private getAttributesForProperty idx elementName (prop: PropertyDefinition) =
    match prop.IsWrappedArray, prop.Type with
    | Some(hasWrapper), CollectionType(itemTy, itemName, _) ->
        let isItemNillable = prop.IsItemNillable |> Option.defaultValue false
        [ CustomAttribute.xrdElement idx elementName prop.QualifiedNamespace prop.IsNillable (not hasWrapper) itemTy.TypeHint
          CustomAttribute.xrdCollection idx (Some(itemName)) None isItemNillable false ]
    | Some(_), _ ->
        failwith "Array should match to CollectionType."
    | None, _ ->
        [ CustomAttribute.xrdElement idx elementName prop.QualifiedNamespace prop.IsNillable false prop.Type.TypeHint ]

/// Build property declarations from property definitions and add them to owner type.
let private addTypeProperties (definitions, subTypes) (ownerTy: ProvidedTypeDefinition) =
    let addTypePropertiesFromDefinition definition =
        // Most of the conditions handle XmlSerializer specific attributes.
        let prop = ownerTy |> addProperty(definition.Name, definition.Type |> cliType, definition.IsOptional)
        definition.Documentation |> Option.iter prop.AddXmlDoc
        let elementName = if prop.Name <> definition.Name then Some(definition.Name) else None
        if definition.IsIgnored then
            prop.AddCustomAttribute(CustomAttribute.xmlIgnore())
        elif definition.Type = AnyType then
            prop.AddCustomAttribute(CustomAttribute.xmlAnyElement())
        elif definition.IsAttribute then
            prop.AddCustomAttribute(CustomAttribute.xmlAttribute())
        else
            definition |> getAttributesForProperty None elementName |> List.iter prop.AddCustomAttribute
    definitions |> List.iter addTypePropertiesFromDefinition
    // Add extra types to owner type declaration.
    ownerTy.AddMembers(subTypes)

let private buildEnumerationType (spec: SimpleTypeRestrictionSpec, itemType) (providedTy: ProvidedTypeDefinition) =
    let enumerationValues = spec.Content |> List.choose (function Enumeration(value) -> Some(value) | _ -> None)
    let initCtor = providedTy |> addContentProperty("BaseValue", itemType, enumerationValues.Length > 0)
    let initializerExpr (value: string) =
        let valueExpr =
            match itemType with
            | PrimitiveType(_, TypeHint.Int) -> Expr.Value(Convert.ToInt32(value))
            | _ -> Expr.Value(value)
        Expr.NewObject(initCtor, [ valueExpr ])
    if enumerationValues.Length > 0 then
        let initExpr =
            enumerationValues
            |> List.map (fun value ->
                let fieldName = String.asValidIdentifierName value
                let field = ProvidedField(fieldName, providedTy)
                field.SetFieldAttributes(FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly)
                providedTy.AddMember(field)
                Expr.FieldSet(field, initializerExpr value))
            |> List.reduce (fun a b -> Expr.Sequential(a, b))
        let staticCtor = ProvidedConstructor([], (fun _ -> initExpr), IsTypeInitializer = true)
        providedTy.AddMember(staticCtor)

let getChoiceInterface len =
    match len with
    | 1 -> Some(typedefof<IChoiceOf1<_>>)
    | 2 -> Some(typedefof<IChoiceOf2<_,_>>)
    | 3 -> Some(typedefof<IChoiceOf3<_,_,_>>)
    | 4 -> Some(typedefof<IChoiceOf4<_,_,_,_>>)
    | 5 -> Some(typedefof<IChoiceOf5<_,_,_,_,_>>)
    | 6 -> Some(typedefof<IChoiceOf6<_,_,_,_,_,_>>)
    | 7 -> Some(typedefof<IChoiceOf7<_,_,_,_,_,_,_>>)
    | 8 -> Some(typedefof<IChoiceOf8<_,_,_,_,_,_,_,_>>)
    | _ -> None

/// Collects property definitions from every content element of complexType.
let rec private collectComplexTypeContentProperties choiceNameGen seqNameGen context (spec: ComplexTypeContentSpec) =
    // Attribute definitions
    let attributeProperties, attrTypes = spec.Attributes |> List.fold (fun (xs, ys) n -> let x, y = n |> buildAttributeProperty context in x::xs, y |> List.append ys) ([], [])
    // Element definitions
    let elementProperties, elemTypes =
        match spec.Content with
        | Some(All(spec)) ->
            if spec.MaxOccurs <> 1u then failwithf "Invalid `maxOccurs` value '%d' specified." spec.MaxOccurs
            if spec.MinOccurs > 1u then failwithf "Invalid `minOccurs` value '%d' specified." spec.MinOccurs
            spec.Elements
            |> List.map (buildElementProperty context (spec.MinOccurs = 0u))
            |> List.unzip
            |> (fun (a, b) -> a, b |> List.collect id)
        | Some(ComplexTypeParticle.Sequence(spec)) ->
            if spec.MinOccurs > 1u || spec.MaxOccurs <> 1u then failwith "not implemented"
            let collectSequenceProperties content =
                match content with
                | Choice(cspec) -> let x, ts = collectChoiceProperties choiceNameGen context cspec in [x], ts
                | Element(spec) -> let x, ts = buildElementProperty context false spec in [x], ts
                | Sequence(sspec) -> (collectSequenceProperties seqNameGen context sspec), []
                | Any -> [ buildAnyProperty() ], []
                | Group -> failwith "Not implemented: group in complexType sequence."
            spec.Content |> List.fold (fun (xs, ys) n -> let x, y = n |> collectSequenceProperties in x |> List.append xs, y |> List.append ys) ([], [])
        | Some(ComplexTypeParticle.Choice(cspec)) ->
            let prop, types = collectChoiceProperties choiceNameGen context cspec
            [prop], types
        | Some(ComplexTypeParticle.Group) ->
            failwith "Not implemented: group in complexType."
        | None -> [], []
    (List.concat [attributeProperties; elementProperties], List.concat [attrTypes; elemTypes])

/// Create single property definition for given element-s schema specification.
and private buildElementProperty (context: TypeBuilderContext) (forceOptional: bool) (spec: ElementSpec) : PropertyDefinition * ProvidedTypeDefinition list =
    let dspec, schemaType = context.DereferenceElementSpec(spec)
    let name = dspec.Name |> Option.get
    buildPropertyDef schemaType spec.MaxOccurs name dspec.Namespace spec.IsNillable (forceOptional || spec.MinOccurs = 0u) context (annotationToText context spec.Annotation) spec.ExpectedContentTypes.IsSome

/// Create single property definition for given attribute-s schema specification.
and private buildAttributeProperty (context: TypeBuilderContext) (spec: AttributeSpec) : PropertyDefinition * ProvidedTypeDefinition list =
    let name, typeDefinition = context.GetAttributeDefinition(spec)
    // Resolve schema type for attribute:
    let schemaType =
        match typeDefinition with
        | Definition(simpleTypeSpec) -> Definition(SimpleDefinition(simpleTypeSpec))
        | Name(name) -> Name(name)
    let isOptional = match spec.Use with Required -> true | _ -> false
    let prop, types = buildPropertyDef schemaType 1u name None false isOptional context (annotationToText context spec.Annotation) false
    { prop with IsAttribute = true }, types

/// Build default property definition from provided schema information.
and private buildPropertyDef schemaType maxOccurs name qualifiedNamespace isNillable isOptional context doc useXop : PropertyDefinition * ProvidedTypeDefinition list =
    match schemaType with
    | Definition(ArrayContent itemSpec) ->
        match context.DereferenceElementSpec(itemSpec) with
        | dspec, Name(n) ->
            let itemName = dspec.Name |> Option.get
            let itemTy = context.GetRuntimeType(SchemaType(n)) |> fixContentType useXop
            ({ PropertyDefinition.Create(name, qualifiedNamespace, isOptional, doc) with
                Type = CollectionType(itemTy, itemName, None)
                IsNillable = isNillable
                IsItemNillable = Some(itemSpec.IsNillable)
                IsWrappedArray = Some(true) }, [])
        | dspec, Definition(def) ->
            let itemName = dspec.Name |> Option.get
            let suffix = itemName |> String.asValidIdentifierName |> String.capitalize
            let typ = ProvidedTypeDefinition((name |> String.asValidIdentifierName) + suffix, Some typeof<obj>, isErased=false)
            typ.AddCustomAttribute(CustomAttribute.xrdAnonymousType LayoutKind.Sequence)
            let runtimeType = ProvidedType(typ)
            buildSchemaType context runtimeType def
            ({ PropertyDefinition.Create(name, qualifiedNamespace, isOptional, doc) with
                Type = CollectionType(runtimeType, itemName, None)
                IsNillable = isNillable
                IsItemNillable = Some(itemSpec.IsNillable)
                IsWrappedArray = Some(true) }, [typ])
    | Definition(def) ->
        let subTy = ProvidedTypeDefinition((name |> String.asValidIdentifierName) + "Type", Some typeof<obj>, isErased=false)
        subTy.AddCustomAttribute(CustomAttribute.xrdAnonymousType LayoutKind.Sequence)
        let runtimeType = ProvidedType(subTy)
        buildSchemaType context runtimeType def
        if maxOccurs > 1u then
            ({ PropertyDefinition.Create(name, qualifiedNamespace, false, doc) with
                Type = CollectionType(runtimeType, name, None)
                IsNillable = isNillable
                IsWrappedArray = Some(false) }, [subTy])
        else
            ({ PropertyDefinition.Create(name, qualifiedNamespace, isOptional, doc) with
                Type = runtimeType
                IsNillable = isNillable }, [subTy])
    | Name(n) ->
        match context.GetRuntimeType(SchemaType(n)) with
        | x when maxOccurs > 1u ->
            ({ PropertyDefinition.Create(name, qualifiedNamespace, false, doc) with
                Type = CollectionType(x |> fixContentType useXop, name, None)
                IsNillable = isNillable
                IsWrappedArray = Some(false) }, [])
        | PrimitiveType(x, thv) when x.IsValueType ->
            ({ PropertyDefinition.Create(name, qualifiedNamespace, isOptional, doc) with
                Type = PrimitiveType((if isNillable then typedefof<Nullable<_>>.MakeGenericType(x) else x), thv)
                IsNillable = isNillable }, [])
        | x ->
            ({ PropertyDefinition.Create(name, qualifiedNamespace, isOptional, doc) with
                Type = x |> fixContentType useXop
                IsNillable = isNillable }, [])

/// Create property definitions for sequence element specification.
and private collectSequenceProperties _ _ _ : PropertyDefinition list =
    []

/// Create property definitions for choice element specification.
and collectChoiceProperties choiceNameGenerator context spec : PropertyDefinition * ProvidedTypeDefinition list =
    let idField = ProvidedField("__id", typeof<int>)
    let valueField = ProvidedField("__value", typeof<obj>)

    let choiceName = choiceNameGenerator()
    let choiceType = ProvidedTypeDefinition(sprintf "%sType" choiceName, Some typeof<obj>, isErased=false)

    let ctor =
        ProvidedConstructor(
            [ ProvidedParameter("id", typeof<int>); ProvidedParameter("value", typeof<obj>) ],
            MethodAttributes.Private ||| MethodAttributes.RTSpecialName,
            (fun args ->
                Expr.Sequential(
                    Expr.FieldSet(Expr.Coerce(args.[0], choiceType), idField, args.[1]),
                    Expr.FieldSet(Expr.Coerce(args.[0], choiceType), valueField, args.[2])
                )
            )
        )

    choiceType.AddCustomAttribute(CustomAttribute.xrdAnonymousType LayoutKind.Choice)
    choiceType.AddMember(idField)
    choiceType.AddMember(valueField)
    choiceType.AddMember(ctor)

    let createOptionType name (propList: PropertyDefinition list) =
        let optionType = ProvidedTypeDefinition(sprintf "%sType" name, Some typeof<obj>, isErased=false)
        optionType.AddCustomAttribute(CustomAttribute.xrdAnonymousType LayoutKind.Sequence)
        optionType |> addTypeProperties (propList, [])
        optionType

    let addTryMethod (id: int) (methName: string) (ty: Type) =
        let optionalType = ProvidedTypeBuilder.MakeGenericType(typedefof<Optional.Option<_>>, [ty])
        let tryGetValue =
            match <@ OptionalHelpers.tryGetValue<string> 1 1 "" @> with
            | Patterns.Call(_, mi, _) -> ProvidedTypeBuilder.MakeGenericMethod(mi.GetGenericMethodDefinition(), [ty])
            | _ -> failwith "never"
        let tryMethod =
            ProvidedMethod(
                methName,
                [],
                optionalType,
                invokeCode=(fun args -> Expr.Call(tryGetValue, [Expr.FieldGet(Expr.Coerce(args.[0], choiceType), idField); Expr.Value(id); Expr.FieldGet(Expr.Coerce(args.[0], choiceType), valueField)]))
            )
        choiceType.AddMember(tryMethod)
        tryMethod

    let addNewMethod id (name: string) (ty: Type) =
        let newMethod =
            ProvidedMethod(
                sprintf "New%s%s" (if Char.IsLower(name.[0]) then "_" else "") name,
                [ProvidedParameter("value", ty)],
                choiceType,
                isStatic=true,
                invokeCode=(fun args -> Expr.NewObject(ctor, [Expr.Value(id); Expr.Coerce(args.[0], typeof<obj>)]))
            )
        choiceType.AddMember(newMethod)

    let choiceInterfaceTypeArguments = ResizeArray<Type * ProvidedMethod>()
    let optionNameGenerator = nameGenerator (sprintf "%sOption" choiceName)
    let choiceInterface = getChoiceInterface spec.Content.Length

    let addChoiceMethod i (mi: MethodInfo) (t: Type) =
        choiceInterface |> Option.iter (fun iface ->
            let optionalType = ProvidedTypeBuilder.MakeGenericType(typedefof<Optional.Option<_>>, [t])
            let methodName = sprintf "TryGetOption%d" i
            let m = ProvidedMethod(sprintf "%s.%s" iface.Name methodName, [], optionalType, invokeCode=(fun args -> Expr.Call(Expr.Coerce(args.[0], choiceType), mi, [])))
            m.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.Virtual)
            choiceType.AddMember(m)
            choiceInterfaceTypeArguments.Add((t, m))
        )

    let addedTypes =
        spec.Content
        |> List.mapi (fun i choiceContent ->
            let methName (name: string) =
                sprintf "TryGet%s%s" (if Char.IsLower(name.[0]) then "_" else "") name
            match choiceContent with
            | Element(spec) ->
                let prop, types = buildElementProperty context false spec
                prop |> getAttributesForProperty (Some(i + 1)) (Some(prop.Name)) |> List.iter choiceType.AddCustomAttribute
                let propType = prop.Type |> cliType
                addNewMethod (i + 1) prop.Name propType
                let name = methName prop.Name
                let tryMethod = addTryMethod (i + 1) name propType
                addChoiceMethod (i + 1) tryMethod propType
                types
            | Sequence(spec) ->
                let props, types = buildSequenceMembers context spec
                let optionName = optionNameGenerator()
                choiceType.AddCustomAttribute(CustomAttribute.xrdElement (Some(i + 1)) (Some(optionName)) None false true None)
                let optionType = createOptionType optionName props
                addNewMethod (i + 1) optionName optionType
                let name = methName optionName
                let tryMethod = addTryMethod (i + 1) name optionType
                addChoiceMethod (i + 1) tryMethod optionType
                optionType::types
            | Any -> failwith "Not implemented: any in choice."
            | Choice(_) -> failwith "Not implemented: choice in choice."
            | Group -> failwith "Not implemented: group in choice.")
        |> List.collect id

    match choiceInterface with
    | Some(iface) ->
        let genIface = ProvidedTypeBuilder.MakeGenericType(iface, choiceInterfaceTypeArguments |> Seq.map fst |> Seq.toList)
        choiceType.AddInterfaceImplementation(genIface)
        choiceInterfaceTypeArguments |> Seq.map snd |> Seq.iteri (fun i mi -> choiceType.DefineMethodOverride(mi, genIface.GetMethod(sprintf "TryGetOption%d" (i + 1))))
    | None -> ()

    { PropertyDefinition.Create(choiceName, None, false, None) with Type = ProvidedType(choiceType) }, choiceType::addedTypes

/// Extract property definitions for all the elements defined in sequence element.
and private buildSequenceMembers context (spec: ParticleSpec) : PropertyDefinition list * ProvidedTypeDefinition list =
    spec.Content
    |> List.map (function
        | Any -> failwith "Not implemented: any in sequence."
        | Choice(_) -> failwith "Not implemented: choice in sequence."
        | Element(espec) -> buildElementProperty context false espec
        | Group -> failwith "Not implemented: group in sequence."
        | Sequence(_) -> failwith "Not implemented: sequence in sequence.")
    |> List.unzip
    |> (fun (a, b) -> a, b |> List.collect id)

and private buildSchemaType (context: TypeBuilderContext) runtimeType schemaType =
    // Extract type declaration from runtime type definition.
    let providedTy = match runtimeType with ProvidedType(providedTy) -> providedTy | _ -> failwith "Only generated types are accepted as arguments!"
    // Generates unique type name for every choice element.
    let choiceNameGen = nameGenerator "Choice"
    let seqNameGen = nameGenerator "Seq"
    // Parse schema definition and add all properties that are defined.
    match schemaType with
    | SimpleDefinition(SimpleTypeSpec.Restriction(spec, annotation)) ->
        annotationToText context annotation |> Option.iter providedTy.AddXmlDoc
        match context.GetRuntimeType(SchemaType(spec.Base)) with
        | ContentType(_)
        | PrimitiveType(_) as rtyp -> providedTy |> buildEnumerationType (spec, rtyp)
        | _ -> failwith "Simple types should not restrict complex types."
    | SimpleDefinition(ListDef) ->
        failwith "Not implemented: list in simpleType."
    | SimpleDefinition(Union(_)) ->
        failwith "Not implemented: union in simpleType."
    | ComplexDefinition(spec) ->
        // Abstract types will have only protected constructor.
        if spec.IsAbstract then
            providedTy.SetAttributes((providedTy.AttributesRaw &&& ~~~TypeAttributes.Sealed) ||| TypeAttributes.Abstract)
            annotationToText context spec.Annotation |> Option.iter providedTy.AddXmlDoc
            providedTy.AddMember(ProvidedConstructor([], MethodAttributes.Family ||| MethodAttributes.RTSpecialName, (fun _ -> <@@ () @@>)))
        else providedTy.AddMember(ProvidedConstructor([], fun _ -> <@@ () @@>))
        // Handle complex type content and add properties for attributes and elements.
        let specContent =
            match spec.Content with
            | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                match context.GetRuntimeType(SchemaType(spec.Base)) with
                | PrimitiveType(_)
                | ContentType(_) as rtyp ->
                    let prop = providedTy |> addProperty("BaseValue", rtyp |> cliType, false)
                    prop.AddCustomAttribute(CustomAttribute.xrdElement None None None false true rtyp.TypeHint)
                    Some(spec.Content)
                | _ ->
                    failwith "ComplexType-s simpleContent should not extend complex types."
            | SimpleContent(SimpleContentSpec.Restriction(_)) ->
                failwith "Not implemented: restriction in complexType-s simpleContent."
            | ComplexContent(Extension(spec)) ->
                match context.GetRuntimeType(SchemaType(spec.Base)) with
                | ProvidedType(_) as baseTy -> providedTy.SetBaseType(baseTy |> cliType)
                | _ -> failwithf "Only complex types can be inherited! (%A)" spec.Base
                Some(spec.Content)
            | ComplexContent(Restriction(_)) ->
                failwith "Not implemented: restriction in complexType-s complexContent"
            | Particle(spec) ->
                Some(spec)
            | Empty ->
                None
        specContent |> Option.iter (fun content -> providedTy |> addTypeProperties (collectComplexTypeContentProperties choiceNameGen seqNameGen context content))
    | EmptyDefinition -> ()

let removeFaultDescription (definition: SchemaTypeDefinition) =
    let isFault content =
        let areFaultElements (el1: ElementSpec) (el2: ElementSpec) =
            el1.Name = Some("faultCode") && el2.Name = Some("faultString")
        match content with
        | Sequence({ Content = [Element(el1); Element(el2)] }) -> areFaultElements el1 el2 || areFaultElements el2 el1
        | _ -> false
    let filterFault (particles: ParticleContent list) =
        particles |> List.filter (isFault >> not)
    match definition with
    | ComplexDefinition({ Content = Particle({ Content = Some(ComplexTypeParticle.Sequence(sequence)) } as particle) } as spec) ->
        let newParticle =
            match sequence.Content with
            | [ Choice(choice) ] ->
                match choice.Content |> filterFault with
                | [Sequence(content)] -> ComplexTypeParticle.Sequence(content)
                | [] | [_] as content -> ComplexTypeParticle.Sequence({ choice with Content = content })
                | content -> ComplexTypeParticle.Choice({ choice with Content = content })
            | content -> ComplexTypeParticle.Sequence({ sequence with Content = filterFault content })
        ComplexDefinition({ spec with Content = Particle({ particle with Content = Some(newParticle) }) })
    | EmptyDefinition | ComplexDefinition(_) | SimpleDefinition(_) -> definition

let buildResponseElementType (context: TypeBuilderContext) (elementName: XName) =
    let elementSpec = elementName |> context.GetElementSpec
    match elementSpec.Definition with
    | Explicit(typeDefinition) ->
        match typeDefinition with
        | Definition(definition) ->
            let runtimeType = context.GetOrCreateType(SchemaElement(elementName))
            definition |> removeFaultDescription |> buildSchemaType context runtimeType
            runtimeType
        | Name(typeName) ->
            context.GetRuntimeType(SchemaType(typeName))
    | Reference(_) -> failwith "Root level element references are not allowed."

/// Build content for each individual service call method.
let private buildServiceType (context: TypeBuilderContext) targetNamespace (operation: ServicePortMethod) : MemberInfo list =
    let additionalMembers = ResizeArray<MemberInfo>()

    let parameters = ResizeArray<ProvidedParameter>()
    parameters.Add(ProvidedParameter("header", typeof<XRoadHeader>))

    let customAttributes = ResizeArray<CustomAttributeData>()
    customAttributes.Add(CustomAttribute.xrdOperation operation.Name operation.Version)
    customAttributes.Add(CustomAttribute.xrdRequiredHeaders XmlNamespace.XRoad operation.InputParameters.RequiredHeaders)

    let addDocLiteralWrappedParameters (spec: ElementSpec) =
        let choiceNameGen = nameGenerator (sprintf "%sChoiceArg" operation.Name)
        let argNameGen = nameGenerator "choiceArg"
        match context.DereferenceElementSpec(spec) |> snd |> context.GetSchemaTypeDefinition with
        | EmptyDefinition -> ()
        | ComplexDefinition({ IsAbstract = false; Content = Particle({ Content = Some(ComplexTypeParticle.Sequence({ Content = content; MinOccurs = 1u; MaxOccurs = 1u })) }) }) ->
            content
            |> List.iter (fun value ->
                match value with
                | Element(elementSpec) ->
                    let dspec, schemaType = context.DereferenceElementSpec(elementSpec)
                    let name = dspec.Name |> Option.get
                    let runtimeType =
                        match schemaType with
                        | Definition(definition) ->
                            let subTy = ProvidedTypeDefinition(sprintf "%s_%sType" operation.Name name, Some typeof<obj>, isErased=false)
                            subTy.AddCustomAttribute(CustomAttribute.xrdAnonymousType LayoutKind.Sequence)
                            let ns = context.GetOrCreateNamespace(targetNamespace)
                            ns.AddMember(subTy)
                            let runtimeType = ProvidedType(subTy)
                            buildSchemaType context runtimeType definition
                            runtimeType
                        | Name(typeName) ->
                            context.GetRuntimeType(SchemaType(typeName)) |> fixContentType dspec.ExpectedContentTypes.IsSome
                    let isOptional = dspec.MinOccurs = 0u
                    let ty = cliType runtimeType
                    let ty = if isOptional then ProvidedTypeBuilder.MakeGenericType(typedefof<Optional.Option<_>>, [ty]) else ty
                    let parameter = ProvidedParameter(name, ty)
                    parameter.AddCustomAttribute(CustomAttribute.xrdElement None None dspec.Namespace false false runtimeType.TypeHint)
                    if isOptional then parameter.AddCustomAttribute(CustomAttribute.optional())
                    parameters.Add(parameter)
                | Choice(particleSpec) ->
                    let def, addedTypes = collectChoiceProperties choiceNameGen context particleSpec
                    let argName = argNameGen()
                    let ty = cliType def.Type
                    let ty = if def.IsOptional then ProvidedTypeBuilder.MakeGenericType(typedefof<Optional.Option<_>>, [ty]) else ty
                    let parameter = ProvidedParameter(argName, ty)
                    //def.Documentation |> Option.iter parameter.AddXmlDoc
                    parameter.AddCustomAttribute(CustomAttribute.xrdElement None None None def.IsNillable false def.Type.TypeHint)
                    parameters.Add(parameter)
                    additionalMembers.AddRange(addedTypes |> Seq.cast<_>)
                | _ -> failwithf "%A" value)
        | _ -> failwithf "Input wrapper element must be defined as complex type that is a sequence of elements (erroneous XML Schema entity `%s`)." (spec.Name |> Option.defaultValue "<unknown>")

    match operation.InputParameters with
    | DocLiteralWrapped(name, content) ->
        customAttributes.Add(CustomAttribute.xrdRequest name.LocalName name.NamespaceName false content.HasMultipartContent)
        name |> context.GetElementSpec |> addDocLiteralWrappedParameters
    | _ -> failwithf "Unsupported message style/encoding '%A'. Only document/literal is supported at the moment." operation.InputParameters

    // buildOperationOutput context operation protocol result |> ignore
    let returnType, invokeCode =
        match operation.OutputParameters with
        | DocLiteralWrapped(name, content) ->
            let elementType = buildResponseElementType context name
            let returnType = cliType elementType
            let result =
                match elementType with
                | CollectionType(itemTy, itemName, _) ->
                    let elementSpec = name |> context.GetElementSpec
                    let itemTy = itemTy |> fixContentType elementSpec.ExpectedContentTypes.IsSome
                    let resultClass = ProvidedTypeDefinition(sprintf "%sResult" operation.Name, Some typeof<obj>, isErased=false)
                    resultClass.SetAttributes(TypeAttributes.NestedPrivate ||| TypeAttributes.Sealed)
                    resultClass.AddCustomAttribute(CustomAttribute.xrdAnonymousType LayoutKind.Sequence)
                    let prop = resultClass |> addProperty ("response", returnType, false)
                    prop.AddCustomAttribute(CustomAttribute.xrdElement None None None false true itemTy.TypeHint)
                    prop.AddCustomAttribute(CustomAttribute.xrdCollection None (Some(itemName)) None false false)
                    Some(prop, resultClass)
                | _ -> None
            let invokeCode =
                let mi = match <@ Protocol.XRoadUtil.MakeServiceCall(Unchecked.defaultof<AbstractEndpointDeclaration>, "", null, [||]) @> with Patterns.Call(_, mi, _) -> mi | _ -> failwith "never"
                match result with
                | Some(prop, cls) ->
                    let ctr = ProvidedTypeDefinition(cls.Name, Some typeof<obj>, isErased=false)
                    customAttributes.Add(CustomAttribute.xrdResponse name.LocalName name.NamespaceName false content.HasMultipartContent (Some (upcast ctr)))
                    (fun (args: Expr list) ->
                        Expr.PropertyGet(
                            Expr.Coerce(
                                Expr.Call(mi, [Expr.Coerce(args.[0], typeof<AbstractEndpointDeclaration>); Expr.Value(operation.Name); args.[1]; Expr.NewArray(typeof<obj>, args |> List.skip 2 |> List.map (fun x -> Expr.Coerce(x, typeof<obj>)))]),
                                ctr
                            ),
                            prop
                        )
                    )
                | None ->
                    customAttributes.Add(CustomAttribute.xrdResponse name.LocalName name.NamespaceName false content.HasMultipartContent None)
                    (fun (args: Expr list) ->
                        Expr.Coerce(
                            Expr.Call(mi, [Expr.Coerce(args.[0], typeof<AbstractEndpointDeclaration>); Expr.Value(operation.Name); args.[1]; Expr.NewArray(typeof<obj>, args |> List.skip 2 |> List.map (fun x -> Expr.Coerce(x, typeof<obj>)))]),
                            returnType
                        )
                    )
            (returnType, invokeCode)
        | _ -> failwithf "Unsupported message style/encoding '%A'. Only document/literal is supported at the moment." operation.InputParameters

    let parameters = parameters |> Seq.toList
    let customAttributes = customAttributes |> Seq.toList

    let providedMethod = ProvidedMethod(operation.Name, parameters, returnType, invokeCode)
    operation.Documentation |> Option.iter providedMethod.AddXmlDoc

    additionalMembers.Add(providedMethod)
    customAttributes |> List.iter providedMethod.AddCustomAttribute

    additionalMembers |> Seq.toList


/// Builds all types, namespaces and services for give producer definition.
/// Called by type provider to retrieve assembly details for generated types.
let buildServiceTypeMembers (schema: ProducerDescription) = [
    // Initialize type and schema element lookup context.
    let context = {
        CachedNamespaces = Dictionary<_, _>()
        CachedTypes = Dictionary<_, _>()
        Attributes = schema |> initCache (fun x -> x.Attributes)
        Elements = schema |> initCache (fun x -> x.Elements)
        Types = schema |> initCache (fun x -> x.Types)
        LanguageCode = schema.LanguageCode
    }

    // Create stubs for each type before building them, because of circular dependencies.
    schema.TypeSchemas
    |> Map.toList
    |> List.iter (fun (_,typeSchema) ->
        typeSchema.Types
        |> Seq.map (fun kvp -> SchemaType(kvp.Key))
        |> Seq.iter (context.GetOrCreateType >> ignore))

    // Build all global types for each type schema definition.
    schema.TypeSchemas
    |> Map.toSeq
    |> Seq.collect (fun (_, typeSchema) -> typeSchema.Types)
    |> Seq.choose (fun x ->
        match context.GetRuntimeType(SchemaType(x.Key)) with
        | CollectionType(prtyp, _, Some(st)) -> Some(prtyp, st)
        | CollectionType(_, _, None) -> None
        | rtyp -> Some(rtyp, x.Value))
    |> Seq.iter (fun args -> args ||> buildSchemaType context)

    // Create base type which holds types generated from all provided schema-s.
    let serviceTypesTy = ProvidedTypeDefinition("DefinedTypes", Some typeof<obj>, isErased=false)

    // Create methods for all operation bindings.
    yield! schema.Services |> List.map (fun service ->
        let serviceTy = ProvidedTypeDefinition(service.Name, Some typeof<obj>, isErased=false)

        service.Ports
        |> List.iter (fun port ->
            let baseCtor = typeof<AbstractEndpointDeclaration>.GetConstructors(BindingFlags.Instance ||| BindingFlags.Public).[0]
            let uriCtor = match <@ Uri("") @> with Patterns.NewObject(c, _) -> c | _ -> failwith "never"

            let ctor0 =
                if Uri.IsWellFormedUriString(port.Uri, UriKind.Absolute) then
                    let ctor = ProvidedConstructor([], invokeCode=(fun _ -> <@@ () @@>))
                    ctor.BaseConstructorCall <- fun args -> baseCtor, [args.[0]; Expr.NewObject(uriCtor, [Expr.Value(port.Uri)])]
                    Some(ctor)
                else None

            let ctor2 =
                let ctor = ProvidedConstructor([ProvidedParameter("uri", typeof<string>)], invokeCode=(fun _ -> <@@ () @@>))
                ctor.BaseConstructorCall <- fun args -> baseCtor, [args.[0]; Expr.NewObject(uriCtor, [args.[1]])]
                ctor

            let ctor3 =
                let ctor = ProvidedConstructor([ProvidedParameter("uri", typeof<Uri>)], invokeCode=(fun _ -> <@@ () @@>))
                ctor.BaseConstructorCall <- fun args -> baseCtor, args
                ctor

            let portTy =
                let portName = if port.Name = service.Name then sprintf "%sPort" port.Name else port.Name
                let portTy = ProvidedTypeDefinition(portName, Some typeof<obj>, isErased=false)
                portTy.SetBaseType(typeof<AbstractEndpointDeclaration>)
                port.Documentation |> Option.iter portTy.AddXmlDoc
                ctor0 |> Option.iter portTy.AddMember
                portTy.AddMember(ctor2)
                portTy.AddMember(ctor3)
                portTy

            port.Methods |> List.collect (buildServiceType context service.Namespace) |> portTy.AddMembers

            serviceTy.AddMember(portTy)
        )

        serviceTy
    )

    // Add types of all the type namespaces.
    context.CachedNamespaces.Values |> Seq.toList |> serviceTypesTy.AddMembers

    yield serviceTypesTy
]
