module internal FSharp.Data.XRoad.Builder

open FSharp.Data.XRoad.Attributes
open FSharp.Data.XRoad.Schema
open FSharp.Data.XRoad.Wsdl
open ProviderImplementation.ProvidedTypes
open System
open System.Collections.Generic
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
(*
    /// Get type name reference for this instance.
    member this.AsCodeTypeReference(?readonly, ?optional): CodeTypeReference =
        let readonly = match readonly with Some(true) -> "readonly " | _ -> ""
        let ctr =
            match this with
            | AnyType -> CodeTypeReference(typeof<System.Xml.Linq.XElement[]>)
            | PrimitiveType(typ, _) -> CodeTypeReference(typ)
            | ProvidedType(_,name) -> CodeTypeReference(readonly + name)
            | CollectionType(typ,_,_) -> CodeTypeReference(typ.AsCodeTypeReference(), 1)
            | ContentType(_) -> CodeTypeReference(typeof<BinaryContent>)
            | UnitType -> CodeTypeReference(typeof<Void>)
        match optional with
        | Some(true) ->
            let optionalType = CodeTypeReference(typedefof<Optional.Option<_>>)
            optionalType.TypeArguments.Add(ctr) |> ignore
            optionalType
        | _ -> ctr
    member this.TypeHint
        with get() =
            match this with
            | ContentType(TypeHint.None)
            | PrimitiveType(_, TypeHint.None) -> None
            | ContentType(thv)
            | PrimitiveType(_, thv) -> Some(thv)
            | _ -> None
*)

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
    open System.Reflection

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

[<AutoOpen>]
module internal Helpers =
    open System.Text.RegularExpressions

    let (|ProducerName|_|) ns =
        match Regex.Match(ns, @"^https?://((?<producer>\w+)\.x-road\.eu(/(?<path>.+)?)?)$") with
        | m when m.Success ->
            let suffix = if m.Groups.["path"].Success then m.Groups.["path"].Value |> String.xmlNamespaceToClassName |> sprintf "_%s" else ""
            Some(sprintf "%s%s" m.Groups.["producer"].Value suffix)
        | _ -> None

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

(*
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
*)

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

(*
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
*)

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

    // Create base type which holds types generated from all provided schema-s.
    let serviceTypesTy = ProvidedTypeDefinition("DefinedTypes", Some typeof<obj>, isErased=false)

    // Add types of all the type namespaces.
    context.CachedNamespaces.Values |> Seq.toList |> serviceTypesTy.AddMembers

    yield serviceTypesTy
]
