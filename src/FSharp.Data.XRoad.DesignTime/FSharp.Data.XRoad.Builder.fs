module internal FSharp.Data.XRoad.Builder

open FSharp.Data.XRoad.Attributes
open FSharp.Data.XRoad.Choices
open FSharp.Data.XRoad.Schema
open FSharp.Data.XRoad.Wsdl
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open System
open System.Collections.Generic
open System.Reflection
open System.Xml.Linq

type ResultBuilder () =
    member __.Bind (v : Result<'a, string list>, f : 'a -> Result<'b, string list>) = Result.bind f v
    member __.Return (v : 'a) : Result<'a, string list> = Ok v
    member __.ReturnFrom (v : Result<'a, string list>) : Result<'a, string list> = v

let res = ResultBuilder ()

module ModifyType =
    type F = ProvidedTypeDefinition -> unit
    let addCustomAttribute attr : F =
        (fun x -> x.AddCustomAttribute(attr))
    let addMember m : F =
        (fun x -> x.AddMember(m))
    let addInterfaceImplementation i : F =
        (fun x -> x.AddInterfaceImplementation(i))
    let defineMethodOverride body decl : F =
        (fun x -> x.DefineMethodOverride(body, decl))
    let addMembers ms : F =
        (fun x -> x.AddMembers(ms))
    let addXmlDoc doc : F =
        (fun x -> x.AddXmlDoc(doc))
    let modifyAttributes (fattr : TypeAttributes -> TypeAttributes) : F =
        (fun x -> x.SetAttributes (fattr x.AttributesRaw))
    let setAttributes attrs : F =
        (fun x -> x.SetAttributes(attrs))
    let setBaseType typ : F =
        (fun x -> x.SetBaseType(typ))

type TypeGenerator (name, ?isSealed) =
    let typ =
        match isSealed with
        | Some(v) ->
            ProvidedTypeDefinition(name, Some typeof<obj>, isErased=false, isSealed=v)
        | None ->
            ProvidedTypeDefinition(name, Some typeof<obj>, isErased=false)
    let steps = ResizeArray<ModifyType.F>()
    member val Name = name with get
    member __.Type with get () : Type = upcast typ
    member __.Modify(step : ModifyType.F) =
        steps.Add(step)
    member __.Build() =
        steps |> Seq.iter ((|>) typ)

/// Type abstraction for code generator.
type RuntimeType =
    /// Represents anonymous type (xs:any definition).
    | AnyType
    /// Represents missing type.
    | UnitType
    /// Simple types that are presented with system runtime types.
    | PrimitiveType of Type * TypeHint
    /// Types that are provided by generated assembly.
    | ProvidedType of TypeGenerator
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

    let asValidIdentifierName (this: string) =
        let propertyName = StringBuilder()
        if not (isIdentifierStartCharacter this.[0]) then
            propertyName.Append("_") |> ignore
        this.ToCharArray()
        |> Array.iter (fun c ->
            if isIdentifierPartCharacter c then
                propertyName.Append(c) |> ignore
            elif propertyName.[propertyName.Length - 1] <> '_'
                then propertyName.Append('_') |> ignore)
        let fixedName = propertyName.ToString()
        if not (isValidIdentifier fixedName) then
            failwithf "Invalid property name `%s`." fixedName
        fixedName

    /// Converts given XML namespace to class name.
    let xmlNamespaceToClassName (this: string) =
        // Remove special symbols from class name.
        let className =
            this.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun p ->
                p.Split([| '.' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.rev
                |> Array.map (fun x -> (x.ToLower() |> CultureInfo.InvariantCulture.TextInfo.ToTitleCase).Replace("-", "") |> asValidIdentifierName)
                |> join "_")
            |> join "_"
        // Check validity of generated class name.
        if not (isValidIdentifier className) then
            failwithf "invalid name %s" className
        className

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
    let rec cliType (runtimeType: RuntimeType) =
        match runtimeType with
        | AnyType -> typeof<XElement[]>
        | PrimitiveType(typ, _) -> typ
        | ProvidedType(tgen) -> tgen.Type
        | CollectionType(typ,_,_) -> (cliType typ).MakeArrayType()
        | ContentType _ -> typeof<BinaryContent>
        | UnitType -> typeof<Void>

    let getProducerName ns =
        let schemaPrefix =
            match Uri.TryCreate(ns, UriKind.Absolute) with
            | true, uri -> uri.GetLeftPart(UriPartial.Scheme)
            | false, _ -> ""
        ns.Substring(schemaPrefix.Length)
        |> String.xmlNamespaceToClassName

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
type internal TypeBuilderContext (schema : ProducerDescription) as this =
    let generatedTypes = ResizeArray<TypeGenerator>()
    // Provided types generated from type schema definitions.
    let cachedTypes = Dictionary<SchemaName, RuntimeType>()

    // Provided types generated to group types from same namespace.
    let cachedNamespaces = Dictionary<XNamespace, ProvidedTypeDefinition>()

    let initCache (selector: SchemaNode -> IDictionary<XName, _>) (schema: ProducerDescription) =
        schema.TypeSchemas
        |> Map.toSeq
        |> Seq.collect (snd >> selector >> Seq.map (fun x -> x.Key.ToString(), x.Value))
        |> Map.ofSeq

    // Schema level attribute definition lookup.
    let attributes = schema |> initCache (fun x -> x.Attributes)

    // Schema level element definition lookup.
    let elements = schema |> initCache (fun x -> x.Elements)

    // Schema level type definition lookup.
    let types = schema |> initCache (fun x -> x.Types)

    // Language code preferred for code comments.
    let languageCode = schema.LanguageCode

    /// Finds element specification from schema-level type lookup.
    let getSchemaType (name: XName) =
        match types.TryFind(name.ToString()) with
        | Some(schemaType) ->
            Ok schemaType
        | None ->
            Error [sprintf "Invalid reference: global type `%A` was not found in current context." name]

    /// Generates new RuntimeType instance depending on given type:
    /// xsd:base64Binary and xsd:hexBinary types represent ContentType.
    /// Types that are mapped to system types represent PrimitiveType value.
    /// Types that have multiplicity larger than 1 are defined as CollectionTypes.
    /// Other types will define separate ProvidedType in generated assembly.
    let createType (name: SchemaName) =
        res {
            match name.XName with
            | BinaryType(thv) ->
                return ContentType(thv)
            | SystemType(args) ->
                return PrimitiveType(args)
            | _ ->
                let nstyp : ProvidedTypeDefinition = this.GetOrCreateNamespace(name.XName.Namespace)
                let! schemaType, schemaTypeName =
                    res {
                        match name with
                        | SchemaElement(xn) ->
                            let std =
                                this.GetElementSpec(xn)
                                |> this.DereferenceElementSpec
                                |> snd
                                |> this.GetSchemaTypeDefinition
                            return (std, None)
                        | SchemaType(xn) ->
                            let! st = getSchemaType(xn)
                            return (st, Some(xn))
                    }
                match schemaTypeName, schemaType with
                | _, ArrayContent (SoapEncArray element)
                | None, ArrayContent (Regular element) ->
                    match this.DereferenceElementSpec(element) with
                    | dspec, Name(xn) ->
                        let itemName = dspec.Name |> Option.get
                        let! rty = this.GetOrCreateType(SchemaType(xn))
                        return CollectionType(rty, itemName, None)
                    | dspec, Definition(def) ->
                        let itemName = dspec.Name |> Option.get
                        let suffix = itemName |> String.asValidIdentifierName |> String.capitalize
                        let tgen = this.GenerateType((String.asValidIdentifierName name.XName.LocalName) + suffix)
                        tgen.Modify(ModifyType.addCustomAttribute (CustomAttribute.xrdAnonymousType LayoutKind.Sequence))
                        nstyp.AddMember(tgen.Type)
                        return CollectionType(ProvidedType(tgen), itemName, Some(def))
                | _ ->
                    let attr, isSealed, typeName =
                        let nm = name.XName.LocalName
                        match name with
                        | SchemaElement _ ->
                            (CustomAttribute.xrdAnonymousType LayoutKind.Sequence, true, sprintf "%sElementType" nm)
                        | SchemaType _ ->
                            (CustomAttribute.xrdType name.XName LayoutKind.Sequence, false, nm)
                    let tgen = this.GenerateType(typeName |> String.asValidIdentifierName, isSealed)
                    tgen.Modify(ModifyType.addCustomAttribute attr)
                    nstyp.AddMember(tgen.Type)
                    return ProvidedType(tgen)
        }

    member __.GeneratedTypes with get () = generatedTypes
    
    member __.NamespaceTypes with get () = cachedNamespaces.Values |> Seq.toList

    /// Find generated type that corresponds to given namespace name.
    /// If type exists, the existing instance is used; otherwise new type is generated.
    member __.GetOrCreateNamespace(nsname: XNamespace) =
        match cachedNamespaces.TryGetValue(nsname) with
        | false, _ ->
            let producerName = getProducerName nsname.NamespaceName
            let typ = ProvidedTypeDefinition(producerName, Some typeof<obj>, isErased=false)
            let namespaceField = ProvidedField.Literal("__TargetNamespace__", typeof<string>, nsname.NamespaceName)
            typ.AddMember(namespaceField)
            cachedNamespaces.Add(nsname, typ)
            typ
        | true, typ -> typ

    /// Get runtime type from cached types if exists; otherwise create the type.
    member __.GetOrCreateType(name: SchemaName) =
        res {
            match cachedTypes.TryGetValue(name) with
            | true, info ->
                return info
            | _ ->
                let! info = createType(name)
                cachedTypes.Add(name, info)
                return info
        }

    /// Get runtime type from cached types if exists.
    member __.GetRuntimeType(name: SchemaName) =
        let resolvedName =
            match name with
            | SchemaElement(xname) ->
                match this.GetElementSpec(xname) with
                | ({ Definition = Explicit(Name(typeName)) } : ElementSpec) -> SchemaType(typeName)
                | _ -> name
            | _ -> name
        match cachedTypes.TryGetValue(resolvedName) with
        | true, typeInfo -> typeInfo
        | _ -> match resolvedName.XName with
               | BinaryType(thv) -> ContentType(thv)
               | SystemType(args) -> PrimitiveType(args)
               | _ -> failwithf "Invalid type name `%A`: type not found in cache." resolvedName

    /// Finds element specification from schema-level element lookup.
    member __.GetElementSpec(name: XName) =
        match elements.TryFind(name.ToString()) with
        | Some(elementSpec) -> elementSpec
        | None -> failwithf "Invalid reference: global element %A was not found in current context." name

    /// Resolves real type definition from lookup by following the XML schema references if present.
    /// Returns value of type definitions which actually contains definition, not references other definition.
    member __.GetSchemaTypeDefinition typeDefinition =
        let rec findSchemaTypeDefinition typeDefinition =
            match typeDefinition with
            | Definition(spec) ->
                spec
            | Name(xn) ->
                match types.TryFind(xn.ToString()) with
                | Some(schemaType) ->
                    schemaType
                | None ->
                    failwithf "Missing referenced schema type `%A`." xn
        findSchemaTypeDefinition typeDefinition

    /// Resolves real atrribute definition from lookup by following the XML schema references if present.
    /// Returns value of attribute definitions which actually contains definition, not references other definition.
    member __.GetAttributeDefinition(spec) =
        let rec findAttributeDefinition (spec: AttributeSpec) =
            match spec.RefOrType with
            | Explicit(typeDefinition) ->
                match spec.Name with
                | Some(name) -> name, typeDefinition
                | None -> failwithf "Attribute has no name."
            | Reference(ref) ->
                match attributes.TryFind(ref.ToString()) with
                | Some(spec) -> findAttributeDefinition(spec)
                | None ->
                    match ref with
                    | XmlName "lang" -> "lang", Name(XName.Get("string", XmlNamespace.Xsd))
                    | _ -> failwithf "Missing referenced attribute %A." ref
        findAttributeDefinition(spec)

    /// Resolves real element definition from lookup by following the XML schema references if present.
    /// Returns value of element definitions which actually contains definition, not references other definition.
    member __.DereferenceElementSpec(spec): ElementSpec * TypeDefinition<SchemaTypeDefinition> =
        let rec findElementDefinition (spec: ElementSpec) =
            match spec.Definition with
            | Explicit(typeDefinition) ->
                match spec.Name with
                | Some _ -> spec, typeDefinition
                | None -> failwithf "Attribute has no name."
            | Reference(ref) ->
                match elements.TryFind(ref.ToString()) with
                | Some(spec) -> findElementDefinition(spec)
                | None -> failwithf "Missing referenced attribute %A." ref
        findElementDefinition(spec)

    member __.AnnotationToText (annotation : Annotation option) =
        annotation
        |> Option.bind (fun annotation ->
            annotation.AppInfo
            |> List.collect (fun e -> e.Elements(XName.Get("title", XmlNamespace.XRoad)) |> List.ofSeq)
            |> List.fold (fun doc el ->
                let lang = el |> Xml.attrOrDefault (XName.Get("lang", XmlNamespace.Xml)) "et"
                (lang, el.Value)::doc) []
            |> List.tryFind (fst >> ((=) languageCode))
            |> Option.map snd)
        
    member __.GenerateType(name, ?isSealed) : TypeGenerator =
        let tgen =
            match isSealed with
            | Some(v) -> TypeGenerator(name, v)
            | None -> TypeGenerator(name)
        generatedTypes.Add(tgen)
        tgen

let fixContentType useXop rtyp =
    match rtyp with
    | ContentType(TypeHint.None) when useXop -> ContentType(TypeHint.Xop)
    | rtyp -> rtyp

/// Create definition of property that accepts any element not defined in schema.
let private buildAnyProperty () =
    { PropertyDefinition.Create("AnyElements", None, false, None) with Type = AnyType }

let nameGenerator name =
    let num = ref 0 in (fun () -> num := !num + 1; sprintf "%s%d" name !num)

/// Add property to given type with backing field.
/// For optional members, extra field is added to notify if property was assigned or not.
let addProperty (name : string, ty: Type, isOptional) (owner: TypeGenerator) =
    let name = name |> String.asValidIdentifierName
    let ty = if isOptional then ProvidedTypeBuilder.MakeGenericType(typedefof<Optional.Option<_>>, [ty]) else ty

    let f = ProvidedField(sprintf "%s__backing" name, ty)
    f.AddCustomAttribute(CustomAttribute.debuggerBrowsable())
    owner.Modify(ModifyType.addMember f)

    let propName = if name = owner.Name then sprintf "%s_" name else name
    let p = ProvidedProperty(propName, ty, getterCode=(fun args -> Expr.FieldGet(Expr.Coerce(args.[0], owner.Type), f)), setterCode=(fun args -> Expr.FieldSetUnchecked(Expr.Coerce(args.[0], owner.Type), f, args.[1])))
    owner.Modify(ModifyType.addMember p)

    p

let addContentProperty (name: string, ty: RuntimeType, predefinedValues) (owner: TypeGenerator) =
    let name = name |> String.asValidIdentifierName
    let systemType = cliType ty

    let f = ProvidedField(name + "__backing", systemType)
    owner.Modify(ModifyType.addMember f)

    let p = ProvidedProperty(name, systemType, getterCode=(fun args -> Expr.FieldGet(Expr.Coerce(args.[0], owner.Type), f)), setterCode=(fun args -> Expr.FieldSet(Expr.Coerce(args.[0], owner.Type), f, args.[1])), isPrivateSetter=true)
    p.AddCustomAttribute(CustomAttribute.xrdElement None None None false true ty.TypeHint)
    owner.Modify(ModifyType.addMember p)

    let ctorAttributes = MethodAttributes.Private  ||| MethodAttributes.RTSpecialName ||| MethodAttributes.HideBySig
    let ctor = ProvidedConstructor([], ctorAttributes, (fun _ -> <@@ () @@>))
    owner.Modify(ModifyType.addMember ctor)

    let var = Var("o", owner.Type)
    let invokeCode : Expr list -> Expr =
        (fun args -> Expr.Let(var, Expr.NewObject(ctor, []), Expr.Sequential(Expr.FieldSet(Expr.Var(var), f, args.[0]), Expr.Var(var))))

    let builderMethod = ProvidedMethod("Create", [ ProvidedParameter("value", systemType) ], owner.Type, invokeCode, true)
    let methodAttributes = (if predefinedValues then MethodAttributes.Private else MethodAttributes.Public) ||| MethodAttributes.Static
    builderMethod.SetMethodAttrs(methodAttributes)
    owner.Modify(ModifyType.addMember builderMethod)

    builderMethod

let private getAttributesForProperty idx elementName (prop: PropertyDefinition) =
    match prop.IsWrappedArray, prop.Type with
    | Some(hasWrapper), CollectionType(itemTy, itemName, _) ->
        let isItemNillable = prop.IsItemNillable |> Option.defaultValue false
        [ CustomAttribute.xrdElement idx elementName prop.QualifiedNamespace prop.IsNillable (not hasWrapper) itemTy.TypeHint
          CustomAttribute.xrdCollection idx (Some(itemName)) None isItemNillable false ]
    | Some _, _ ->
        failwith "Array should match to CollectionType."
    | None, _ ->
        [ CustomAttribute.xrdElement idx elementName prop.QualifiedNamespace prop.IsNillable false prop.Type.TypeHint ]

/// Build property declarations from property definitions and add them to owner type.
let private addTypeProperties (definitions, subTypes) (ownerTy: TypeGenerator) =
    let addTypePropertiesFromDefinition definition =
        // Most of the conditions handle XmlSerializer specific attributes.
        let prop = addProperty(definition.Name, definition.Type |> cliType, definition.IsOptional) ownerTy
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
    ownerTy.Modify(ModifyType.addMembers subTypes)

let private buildEnumerationType (spec: SimpleTypeRestrictionSpec, itemType) (tgen: TypeGenerator) =
    let enumerationValues = spec.Content |> List.choose (function Enumeration(value) -> Some(value) | _ -> None) |> List.distinct
    let builderMethod = addContentProperty("BaseValue", itemType, enumerationValues.Length > 0) tgen
    let initializerExpr (value: string) =
        let valueExpr =
            match itemType with
            | PrimitiveType(_, TypeHint.Int) -> Expr.Value(Convert.ToInt32(value))
            | _ -> Expr.Value(value)
        Expr.Call(builderMethod, [ valueExpr ])
    if enumerationValues.Length > 0 then
        let rec pairwiseSequential (exprs: Expr list) =
            match exprs with
            | [expr] -> expr
            | _ ->
                exprs
                |> List.chunkBySize 2
                |> List.map (fun xs -> match xs with [a] -> a | [a; b] -> Expr.Sequential(a, b) | _ -> failwith "never")
                |> pairwiseSequential
        let initExpr =
            enumerationValues
            |> List.map (fun value ->
                let fieldName =
                    match value with
                    | "" -> "__None__"
                    | _ -> String.asValidIdentifierName value
                let field = ProvidedField(fieldName, tgen.Type)
                field.SetFieldAttributes(FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly)
                tgen.Modify(ModifyType.addMember field)
                Expr.FieldSet(field, initializerExpr value))
            |> pairwiseSequential
        let staticCtor = ProvidedConstructor([], (fun _ -> initExpr), IsTypeInitializer = true)
        tgen.Modify(ModifyType.addMember staticCtor)

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
and private buildElementProperty (context: TypeBuilderContext) (forceOptional: bool) (spec: ElementSpec) : PropertyDefinition * TypeGenerator list =
    let dspec, schemaType = context.DereferenceElementSpec(spec)
    let name = dspec.Name |> Option.get
    buildPropertyDef schemaType spec.MaxOccurs name dspec.Namespace spec.IsNillable (forceOptional || spec.MinOccurs = 0u) context (context.AnnotationToText spec.Annotation) spec.ExpectedContentTypes.IsSome

/// Create single property definition for given attribute-s schema specification.
and private buildAttributeProperty (context: TypeBuilderContext) (spec: AttributeSpec) : PropertyDefinition * TypeGenerator list =
    let name, typeDefinition = context.GetAttributeDefinition(spec)
    // Resolve schema type for attribute:
    let schemaType =
        match typeDefinition with
        | Definition(simpleTypeSpec) -> Definition(SimpleDefinition(simpleTypeSpec))
        | Name(name) -> Name(name)
    let isOptional = match spec.Use with Required -> true | _ -> false
    let prop, types = buildPropertyDef schemaType 1u name None false isOptional context (context.AnnotationToText spec.Annotation) false
    { prop with IsAttribute = true }, types

/// Build default property definition from provided schema information.
and private buildPropertyDef schemaType maxOccurs name qualifiedNamespace isNillable isOptional context doc useXop : PropertyDefinition * TypeGenerator list =
    match schemaType with
    | Definition(ArrayContent (Regular itemSpec | SoapEncArray itemSpec)) ->
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
            let tgen = context.GenerateType((name |> String.asValidIdentifierName) + suffix)
            tgen.Modify(ModifyType.addCustomAttribute (CustomAttribute.xrdAnonymousType LayoutKind.Sequence))
            let runtimeType = ProvidedType(tgen)
            buildSchemaType context runtimeType def
            ({ PropertyDefinition.Create(name, qualifiedNamespace, isOptional, doc) with
                Type = CollectionType(runtimeType, itemName, None)
                IsNillable = isNillable
                IsItemNillable = Some(itemSpec.IsNillable)
                IsWrappedArray = Some(true) }, [tgen])
    | Definition(def) ->
        let tgen = context.GenerateType((name |> String.asValidIdentifierName) + "Type")
        tgen.Modify(ModifyType.addCustomAttribute (CustomAttribute.xrdAnonymousType LayoutKind.Sequence))
        let runtimeType = ProvidedType(tgen)
        buildSchemaType context runtimeType def
        if maxOccurs > 1u then
            ({ PropertyDefinition.Create(name, qualifiedNamespace, false, doc) with
                Type = CollectionType(runtimeType, name, None)
                IsNillable = isNillable
                IsWrappedArray = Some(false) }, [tgen])
        else
            ({ PropertyDefinition.Create(name, qualifiedNamespace, isOptional, doc) with
                Type = runtimeType
                IsNillable = isNillable }, [tgen])
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
and collectChoiceProperties choiceNameGenerator context spec : PropertyDefinition * TypeGenerator list =
    let idField = ProvidedField("__id", typeof<int>)
    let valueField = ProvidedField("__value", typeof<obj>)

    let choiceName = choiceNameGenerator()
    let choiceTgen = context.GenerateType(sprintf "%sType" choiceName)

    let ctor =
        ProvidedConstructor(
            [ ProvidedParameter("id", typeof<int>); ProvidedParameter("value", typeof<obj>) ],
            MethodAttributes.Private ||| MethodAttributes.RTSpecialName ||| MethodAttributes.HideBySig,
            (fun args ->
                Expr.Sequential(
                    Expr.FieldSet(Expr.Coerce(args.[0], choiceTgen.Type), idField, args.[1]),
                    Expr.FieldSet(Expr.Coerce(args.[0], choiceTgen.Type), valueField, args.[2])
                )
            )
        )

    choiceTgen.Modify(CustomAttribute.xrdAnonymousType LayoutKind.Choice |> ModifyType.addCustomAttribute)
    choiceTgen.Modify(ModifyType.addMember idField)
    choiceTgen.Modify(ModifyType.addMember valueField)
    choiceTgen.Modify(ModifyType.addMember ctor)

    let createOptionType name (propList: PropertyDefinition list) =
        let tgen = context.GenerateType(sprintf "%sType" name)
        tgen.Modify(CustomAttribute.xrdAnonymousType LayoutKind.Sequence |> ModifyType.addCustomAttribute)
        addTypeProperties (propList, []) tgen
        tgen

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
                invokeCode=(fun args -> Expr.Call(tryGetValue, [Expr.FieldGet(Expr.Coerce(args.[0], choiceTgen.Type), idField); Expr.Value(id); Expr.FieldGet(Expr.Coerce(args.[0], choiceTgen.Type), valueField)]))
            )
        choiceTgen.Modify(ModifyType.addMember tryMethod)
        tryMethod

    let addNewMethod id (name: string) (ty: Type) =
        let newMethod =
            ProvidedMethod(
                sprintf "New%s%s" (if Char.IsLower(name.[0]) then "_" else "") name,
                [ProvidedParameter("value", ty)],
                choiceTgen.Type,
                isStatic=true,
                invokeCode=(fun args -> Expr.NewObject(ctor, [Expr.Value(id); Expr.Coerce(args.[0], typeof<obj>)]))
            )
        choiceTgen.Modify(ModifyType.addMember newMethod)

    let choiceInterfaceTypeArguments = ResizeArray<Type * ProvidedMethod>()
    let optionNameGenerator = nameGenerator (sprintf "%sOption" choiceName)
    let choiceInterface = getChoiceInterface spec.Content.Length

    let addChoiceMethod i (mi: MethodInfo) (t: Type) =
        choiceInterface |> Option.iter (fun iface ->
            let optionalType = ProvidedTypeBuilder.MakeGenericType(typedefof<Optional.Option<_>>, [t])
            let methodName = sprintf "TryGetOption%d" i
            let m = ProvidedMethod(sprintf "%s.%s" iface.Name methodName, [], optionalType, invokeCode=(fun args -> Expr.Call(Expr.Coerce(args.[0], choiceTgen.Type), mi, [])))
            m.SetMethodAttrs(MethodAttributes.Private ||| MethodAttributes.Virtual)
            choiceTgen.Modify(ModifyType.addMember m)
            choiceInterfaceTypeArguments.Add((t, m))
        )

    let addedTypes : TypeGenerator list =
        spec.Content
        |> List.mapi (fun i choiceContent ->
            let methName (name: string) =
                sprintf "TryGet%s%s" (if Char.IsLower(name.[0]) then "_" else "") name
            match choiceContent with
            | Element(spec) ->
                let prop, types = buildElementProperty context false spec
                prop |> getAttributesForProperty (Some(i + 1)) (Some(prop.Name)) |> List.iter (ModifyType.addCustomAttribute >> choiceTgen.Modify)
                let propType = prop.Type |> cliType
                addNewMethod (i + 1) prop.Name propType
                let name = methName prop.Name
                let tryMethod = addTryMethod (i + 1) name propType
                addChoiceMethod (i + 1) tryMethod propType
                types
            | Sequence(spec) ->
                let props, types = buildSequenceMembers context spec
                let optionName = optionNameGenerator()
                choiceTgen.Modify(ModifyType.addCustomAttribute (CustomAttribute.xrdElement (Some(i + 1)) (Some(optionName)) None false true None))
                let optionType = createOptionType optionName props
                addNewMethod (i + 1) optionName optionType.Type
                let name = methName optionName
                let tryMethod = addTryMethod (i + 1) name optionType.Type
                addChoiceMethod (i + 1) tryMethod optionType.Type
                optionType::types
            | Any -> failwith "Not implemented: any in choice."
            | Choice _ -> failwith "Not implemented: choice in choice."
            | Group -> failwith "Not implemented: group in choice.")
        |> List.collect id

    match choiceInterface with
    | Some(iface) ->
        let genIface = ProvidedTypeBuilder.MakeGenericType(iface, choiceInterfaceTypeArguments |> Seq.map fst |> Seq.toList)
        choiceTgen.Modify(ModifyType.addInterfaceImplementation genIface)
        choiceInterfaceTypeArguments
        |> Seq.map snd
        |> Seq.iteri (fun i mi ->
            let declMi = (genIface.GetMethod(sprintf "TryGetOption%d" (i + 1)))
            choiceTgen.Modify(ModifyType.defineMethodOverride mi declMi))
    | None -> ()

    { PropertyDefinition.Create(choiceName, None, false, None) with Type = ProvidedType(choiceTgen) }, choiceTgen::addedTypes

/// Extract property definitions for all the elements defined in sequence element.
and private buildSequenceMembers context (spec: ParticleSpec) : PropertyDefinition list * TypeGenerator list =
    spec.Content
    |> List.map (function
        | Any -> failwith "Not implemented: any in sequence."
        | Choice _ -> failwith "Not implemented: choice in sequence."
        | Element(espec) -> buildElementProperty context false espec
        | Group -> failwith "Not implemented: group in sequence."
        | Sequence _ -> failwith "Not implemented: sequence in sequence.")
    |> List.unzip
    |> (fun (a, b) -> a, b |> List.collect id)

and private buildSchemaType (context: TypeBuilderContext) runtimeType schemaType =
    // Extract type declaration from runtime type definition.
    let tgen =
        match runtimeType with
        | ProvidedType(providedTy) ->
            providedTy
        | _ ->
            failwith "Only generated types are accepted as arguments!"
    // Generates unique type name for every choice element.
    let choiceNameGen = nameGenerator "Choice"
    let seqNameGen = nameGenerator "Seq"
    // Parse schema definition and add all properties that are defined.
    match schemaType with
    | SimpleDefinition(SimpleTypeSpec.Restriction(spec, annotation)) ->
        context.AnnotationToText annotation |> Option.iter (ModifyType.addXmlDoc >> tgen.Modify)
        match context.GetRuntimeType(SchemaType(spec.Base)) with
        | ContentType _
        | PrimitiveType _ as rtyp ->
            buildEnumerationType (spec, rtyp) tgen
        | _ ->
            failwith "Simple types should not restrict complex types."
    | SimpleDefinition(ListDef) ->
        failwith "Not implemented: list in simpleType."
    | SimpleDefinition(Union _) ->
        failwith "Not implemented: union in simpleType."
    | ComplexDefinition(spec) ->
        // Abstract types will have only protected constructor.
        if spec.IsAbstract then
            tgen.Modify(ModifyType.modifyAttributes (fun attrs -> (attrs &&& ~~~TypeAttributes.Sealed) ||| TypeAttributes.Abstract))
            context.AnnotationToText spec.Annotation |> Option.iter (ModifyType.addXmlDoc >> tgen.Modify)
            tgen.Modify(ModifyType.addMember (ProvidedConstructor([], MethodAttributes.Family ||| MethodAttributes.RTSpecialName ||| MethodAttributes.HideBySig, (fun _ -> <@@ () @@>))))
        else tgen.Modify(ModifyType.addMember (ProvidedConstructor([], fun _ -> <@@ () @@>)))
        // Handle complex type content and add properties for attributes and elements.
        let specContent =
            match spec.Content with
            | SimpleContent(SimpleContentSpec.Extension(spec)) ->
                match context.GetRuntimeType(SchemaType(spec.Base)) with
                | PrimitiveType _
                | ContentType _ as rtyp ->
                    let prop = addProperty ("BaseValue", rtyp |> cliType, false) tgen
                    prop.AddCustomAttribute(CustomAttribute.xrdElement None None None false true rtyp.TypeHint)
                    Some(spec.Content)
                | _ ->
                    failwith "ComplexType-s simpleContent should not extend complex types."
            | SimpleContent(SimpleContentSpec.Restriction _) ->
                failwith "Not implemented: restriction in complexType-s simpleContent."
            | ComplexContent(Extension(spec)) ->
                match context.GetRuntimeType(SchemaType(spec.Base)) with
                | ProvidedType _ as baseTy ->
                    tgen.Modify(ModifyType.setBaseType (baseTy |> cliType))
                | baseTy ->
                    failwithf "Only complex types can be inherited! (%A)" baseTy
                Some(spec.Content)
            | ComplexContent(Restriction(spec)) ->
                // TODO: needs better implementation.
                // failwith "Not implemented: restriction in complexType-s complexContent"
                match context.GetRuntimeType(SchemaType(spec.Base)) with
                | ProvidedType _ as baseTy ->
                    tgen.Modify(ModifyType.setBaseType (baseTy |> cliType))
                | PrimitiveType(_, TypeHint.AnyType) ->
                    ()
                | baseTy ->
                    failwithf "Only complex types can be inherited! (%A)" baseTy
                None
            | Particle(spec) ->
                Some(spec)
            | Empty ->
                None
        specContent
        |> Option.iter (fun content ->
            let (definitions, subTypes) = collectComplexTypeContentProperties choiceNameGen seqNameGen context content
            addTypeProperties (definitions, subTypes |> List.map (fun x -> x.Type)) tgen)
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
    | EmptyDefinition | ComplexDefinition _ | SimpleDefinition _ -> definition

let buildResponseElementType (context: TypeBuilderContext) (elementName: XName) =
    res {
        let elementSpec = elementName |> context.GetElementSpec
        match elementSpec.Definition with
        | Explicit(typeDefinition) ->
            match typeDefinition with
            | Definition(definition) ->
                let! runtimeType = context.GetOrCreateType(SchemaElement(elementName))
                definition |> removeFaultDescription |> buildSchemaType context runtimeType
                return runtimeType
            | Name(typeName) ->
                return context.GetRuntimeType(SchemaType(typeName))
        | Reference _ ->
            return! Error ["Root level element references are not allowed."]
    }

/// Build content for each individual service call method.
let private buildServiceType (context: TypeBuilderContext) targetNamespace (operation: ServicePortMethod) : MemberInfo list =
    let additionalMembers = ResizeArray<MemberInfo>()

    let parameters = ResizeArray<ProvidedParameter>()
    parameters.Add(ProvidedParameter("header", typeof<XRoadHeader>))

    let customAttributes = ResizeArray<CustomAttributeData>()
    customAttributes.Add(CustomAttribute.xrdOperation operation.Name operation.Version)
    customAttributes.Add(CustomAttribute.xrdRequiredHeaders XmlNamespace.XRoad operation.InputParameters.RequiredHeaders)

    let paramDoc = Dictionary<string, string>()

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
                            let tgen = context.GenerateType(sprintf "%s_%sType" operation.Name name)
                            tgen.Modify(CustomAttribute.xrdAnonymousType LayoutKind.Sequence |> ModifyType.addCustomAttribute)
                            let ns = context.GetOrCreateNamespace(targetNamespace)
                            ns.AddMember(tgen.Type)
                            let runtimeType = ProvidedType(tgen)
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
                    def.Documentation |> Option.iter (fun doc -> paramDoc.Add(argName, doc))
                    parameter.AddCustomAttribute(CustomAttribute.xrdElement None None None def.IsNillable false def.Type.TypeHint)
                    parameters.Add(parameter)
                    additionalMembers.AddRange(addedTypes |> Seq.cast<_>)
                | _ -> failwithf "%A" value)
        | _ -> failwithf "Input wrapper element must be defined as complex type that is a sequence of elements (erroneous XML Schema entity `%s`)." (spec.Name |> Option.defaultValue "<unknown>")

    match operation.InputParameters with
    | DocLiteralWrapped(name, content) ->
        customAttributes.Add(CustomAttribute.xrdRequest name.LocalName name.NamespaceName false content.HasMultipartContent)
        name |> context.GetElementSpec |> addDocLiteralWrappedParameters
    | _ ->
        failwithf "Unsupported message style/encoding '%A'. Only document/literal is supported at the moment." operation.InputParameters

    // buildOperationOutput context operation protocol result |> ignore
    // let (returnType, invokeCode) =
    let returnTypeInvokeCodeResult =
        res {
            match operation.OutputParameters with
            | DocLiteralWrapped(name, content) ->
                let! elementType = buildResponseElementType context name
                let returnType = cliType elementType
                let result =
                    match elementType with
                    | CollectionType(itemTy, itemName, _) ->
                        let elementSpec = name |> context.GetElementSpec
                        let itemTy = itemTy |> fixContentType elementSpec.ExpectedContentTypes.IsSome
                        let tgen = context.GenerateType(sprintf "%sResult" operation.Name)
                        tgen.Modify(ModifyType.setAttributes (TypeAttributes.NestedPrivate ||| TypeAttributes.Sealed))
                        tgen.Modify(CustomAttribute.xrdAnonymousType LayoutKind.Sequence |> ModifyType.addCustomAttribute)
                        let prop = addProperty ("response", returnType, false) tgen
                        prop.AddCustomAttribute(CustomAttribute.xrdElement None None None false true itemTy.TypeHint)
                        prop.AddCustomAttribute(CustomAttribute.xrdCollection None (Some(itemName)) None false false)
                        Some(prop, tgen)
                    | _ -> None
                let invokeCode =
                    let mi = match <@ Protocol.XRoadUtil.MakeServiceCall(Unchecked.defaultof<AbstractEndpointDeclaration>, "", null, [||]) @> with Patterns.Call(_, mi, _) -> mi | _ -> failwith "never"
                    match result with
                    | Some(prop, cls) ->
                        let tgen = context.GenerateType(cls.Name)
                        customAttributes.Add(CustomAttribute.xrdResponse name.LocalName name.NamespaceName false content.HasMultipartContent (Some tgen.Type))
                        (fun (args: Expr list) ->
                            Expr.PropertyGet(
                                Expr.Coerce(
                                    Expr.Call(mi, [Expr.Coerce(args.[0], typeof<AbstractEndpointDeclaration>); Expr.Value(operation.Name); args.[1]; Expr.NewArray(typeof<obj>, args |> List.skip 2 |> List.map (fun x -> Expr.Coerce(x, typeof<obj>)))]),
                                    cls.Type
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
                return (returnType, invokeCode)
            | _ ->
                return! Error [sprintf "Unsupported message style/encoding '%A'. Only document/literal is supported at the moment." operation.InputParameters]
        }

    let parameters = parameters |> Seq.toList
    let customAttributes = customAttributes |> Seq.toList
    
    let (returnType, invokeCode) =
        match returnTypeInvokeCodeResult with
        | Ok (a, b) -> (a, b)
        | Error errors -> failwithf "%A" errors

    let providedMethod = ProvidedMethod(operation.Name, parameters, returnType, invokeCode)

    let docBuilder = Text.StringBuilder()
    operation.Documentation |> Option.iter (fun doc -> docBuilder.AppendLine(sprintf "<summary>%s</summary>" doc) |> ignore)
    paramDoc |> Seq.iter (fun kvp -> docBuilder.AppendLine(sprintf "<param name=\"%s\">%s</param>" kvp.Key kvp.Value) |> ignore)
    if docBuilder.Length > 0 then providedMethod.AddXmlDoc (docBuilder.ToString())

    additionalMembers.Add(providedMethod)
    customAttributes |> List.iter providedMethod.AddCustomAttribute

    additionalMembers |> Seq.toList

/// Builds all types, namespaces and services for give producer definition.
/// Called by type provider to retrieve assembly details for generated types.
let buildServiceTypeMembers schema =
    // Initialize type and schema element lookup context.
    let context = TypeBuilderContext(schema)

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
    let methodTypes =
        schema.Services
        |> List.map (fun service ->
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

    context.GeneratedTypes |> Seq.iter (fun tgen -> tgen.Build())

    // Add types of all the type namespaces.
    context.NamespaceTypes |> serviceTypesTy.AddMembers

    serviceTypesTy :: methodTypes
