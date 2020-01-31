﻿module internal FSharp.Data.XRoad.Emit

open FSharp.Data.XRoad.Attributes
open FSharp.Data.XRoad.Extensions
open FSharp.Quotations.Patterns
open System
open System.Collections.Concurrent
open System.Numerics
open System.Reflection
open System.Reflection.Emit
open System.Xml
open System.Xml.Linq

let (!@) expr =
    match expr with
    | Call(_, mi, _) -> mi
    | PropertyGet(_, pi, _) -> pi.GetGetMethod()
    | _ -> failwithf "Must be method call expression, but was `%A`." expr

let (!!@) expr =
    match expr with
    | NewObject(ci, _) -> ci
    | _ -> failwith "Must be constructor expression"

let rec private typeName (typ: Type) =
    if typ |> isNull then "" else
    let name = sprintf "%s%s" (if typ.Namespace |> isNull then "" else typ.Namespace + ".") typ.Name
    if not typ.IsGenericType then name else
    sprintf "%s<%s>" name (String.Join(",", typ.GetGenericArguments() |> Array.map typeName))

let inline declareLocal typ (il: ILGenerator) =
    il.DeclareLocal(typ)

let inline private setLabel label (il: ILGenerator) =
    il.MarkLabel(label)
    il

let inline private emit opCode (il: ILGenerator) =
    il.Emit(opCode)
    il

let inline private emittyp opCode (typ: Type) (il: ILGenerator) =
    il.Emit(opCode, typ)
    il

let inline private emitint opCode i (il: ILGenerator) =
    il.Emit(opCode, int i)
    il

let inline private emitmi opCode (mi: MethodInfo) (il: ILGenerator) =
    il.Emit(opCode, mi)
    il

let inline private emitfld opCode (fi: FieldInfo) (il: ILGenerator) =
    il.Emit(opCode, fi)
    il

let inline private emitlbl opCode (label: Label) (il: ILGenerator) =
    il.Emit(opCode, label)
    il

let inline private emitstr opCode (value: string) (il: ILGenerator) =
    il.Emit(opCode, value)
    il

let inline private emitvar opCode (var: LocalBuilder) (il: ILGenerator) =
    il.Emit(opCode, var)
    il

let inline private callCtor (typ: Type) args (il: ILGenerator) =
    il.Emit(OpCodes.Newobj, typ.GetConstructor(args |> List.toArray))
    il

let inline private create (ci: ConstructorInfo) (il: ILGenerator) =
    il.Emit(OpCodes.Newobj, ci)
    il

let inline declareLocalOf<'T> il = il |> declareLocal(typeof<'T>)

let inline private defineLabel (il: ILGenerator) = il.DefineLabel()

let defineMethod (mi: MethodInfo) f =
    match mi with
    | :? DynamicMethod as dyn -> dyn.GetILGenerator() |> f |> ignore
    | _ -> failwith "Cannot cast to dynamic method."

type Emitter = ILGenerator -> ILGenerator

type EmitBuilder() =
    member __.Bind(_, _) = id
    member __.Return(_) = id
    member __.Zero() = id

type EmitBuilder with
    [<CustomOperation("castclass", MaintainsVariableSpaceUsingBind = true)>]
    member __.CastClass(p: Emitter, t) = p >> emittyp OpCodes.Castclass t
    [<CustomOperation("ldarg_0", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldarg0(p: Emitter) = p >> emit OpCodes.Ldarg_0
    [<CustomOperation("ldarg_1", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldarg1(p: Emitter) = p >> emit OpCodes.Ldarg_1
    [<CustomOperation("ldarg_2", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldarg2(p: Emitter) = p >> emit OpCodes.Ldarg_2
    [<CustomOperation("ldarg_3", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldarg3(p: Emitter) = p >> emit OpCodes.Ldarg_3
    [<CustomOperation("ldarg_4", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldarg4(p: Emitter) = p >> emitint OpCodes.Ldarg 4us
    [<CustomOperation("callvirt", MaintainsVariableSpaceUsingBind = true)>]
    member __.Callvirt(p: Emitter, mi) = p >> emitmi OpCodes.Callvirt mi
    [<CustomOperation("callvirt_expr", MaintainsVariableSpaceUsingBind = true)>]
    member __.CallvirtExpr(p: Emitter, e) = p >> emitmi OpCodes.Callvirt (!@ e)
    [<CustomOperation("ldloc", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldloc(p: Emitter, v) = p >> emitvar OpCodes.Ldloc v
    [<CustomOperation("stloc", MaintainsVariableSpaceUsingBind = true)>]
    member __.Stloc(p: Emitter, v) = p >> emitvar OpCodes.Stloc v
    [<CustomOperation("ceq", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ceq(p: Emitter) = p >> emit OpCodes.Ceq
    [<CustomOperation("ldc_node_type", MaintainsVariableSpaceUsingBind = true)>]
    member __.LdcXmlNodeType(p: Emitter, i: Xml.XmlNodeType) = p >> emitint OpCodes.Ldc_I4 i
    [<CustomOperation("ldc_i4", MaintainsVariableSpaceUsingBind = true)>]
    member __.LdcInt(p: Emitter, i) = p >> emitint OpCodes.Ldc_I4 i
    [<CustomOperation("ldc_i4_0", MaintainsVariableSpaceUsingBind = true)>]
    member __.LdcInt0(p: Emitter) = p >> emit OpCodes.Ldc_I4_0
    [<CustomOperation("ldc_i4_1", MaintainsVariableSpaceUsingBind = true)>]
    member __.LdcInt1(p: Emitter) = p >> emit OpCodes.Ldc_I4_1
    [<CustomOperation("ldc_i4_2", MaintainsVariableSpaceUsingBind = true)>]
    member __.LdcInt2(p: Emitter) = p >> emit OpCodes.Ldc_I4_2
    [<CustomOperation("brfalse", MaintainsVariableSpaceUsingBind = true)>]
    member __.Brfalse(p: Emitter, l) = p >> emitlbl OpCodes.Brfalse l
    [<CustomOperation("brtrue", MaintainsVariableSpaceUsingBind = true)>]
    member __.Brtrue(p: Emitter, l) = p >> emitlbl OpCodes.Brtrue l
    [<CustomOperation("br", MaintainsVariableSpaceUsingBind = true)>]
    member __.Br(p: Emitter, l) = p >> emitlbl OpCodes.Br l
    [<CustomOperation("clt", MaintainsVariableSpaceUsingBind = true)>]
    member __.Clt(p: Emitter) = p >> emit OpCodes.Clt
    [<CustomOperation("nop", MaintainsVariableSpaceUsingBind = true)>]
    member __.Nop(p: Emitter) = p >> emit OpCodes.Nop
    [<CustomOperation("unbox", MaintainsVariableSpaceUsingBind = true)>]
    member __.Unbox(p: Emitter, t) = p >> emittyp OpCodes.Unbox_Any t
    [<CustomOperation("box", MaintainsVariableSpaceUsingBind = true)>]
    member __.Box(p: Emitter, t) = p >> emittyp OpCodes.Box t
    [<CustomOperation("set_marker", MaintainsVariableSpaceUsingBind = true)>]
    member __.MarkLabel(p: Emitter, l) = p >> setLabel l
    [<CustomOperation("define_label", MaintainsVariableSpaceUsingBind = true)>]
    member __.DefineLabel(p: Emitter, e) = p >> (fun il -> let label = il |> defineLabel in il |> e label)
    [<CustomOperation("define_labels", MaintainsVariableSpaceUsingBind = true)>]
    member __.DefineLabels(p: Emitter, n, e) = p >> (fun il -> let labels = [ for _ in 1..n do yield il |> defineLabel ] in il |> e labels)
    [<CustomOperation("declare_variable", MaintainsVariableSpaceUsingBind = true)>]
    member __.DeclareVariable(p: Emitter, (v: Lazy<_>), f) = p >> (fun il -> il |> f (il |> v.Value))
    [<CustomOperation("merge", MaintainsVariableSpaceUsingBind = true)>]
    member __.Merge(p: Emitter, e) = p >> e
    [<CustomOperation("call", MaintainsVariableSpaceUsingBind = true)>]
    member __.Call(p: Emitter, mi) = p >> emitmi OpCodes.Call mi
    [<CustomOperation("call_expr", MaintainsVariableSpaceUsingBind = true)>]
    member __.CallExpr(p: Emitter, e) = p >> emitmi OpCodes.Call (!@ e)
    [<CustomOperation("ldstr", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldstr(p: Emitter, s) = p >> emitstr OpCodes.Ldstr s
    [<CustomOperation("string_equals", MaintainsVariableSpaceUsingBind = true)>]
    member __.StringEquals(p: Emitter) = p >> emitmi OpCodes.Call (!@ <@ "" = "" @>)
    [<CustomOperation("add", MaintainsVariableSpaceUsingBind = true)>]
    member __.Add(p: Emitter) = p >> emit OpCodes.Add
    [<CustomOperation("div", MaintainsVariableSpaceUsingBind = true)>]
    member __.Div(p: Emitter) = p >> emit OpCodes.Div
    [<CustomOperation("ret", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ret(p: Emitter) = p >> emit OpCodes.Ret
    [<CustomOperation("pop", MaintainsVariableSpaceUsingBind = true)>]
    member __.Pop(p: Emitter) = p >> emit OpCodes.Pop
    [<CustomOperation("throw", MaintainsVariableSpaceUsingBind = true)>]
    member __.Throw(p: Emitter) = p >> emit OpCodes.Throw
    [<CustomOperation("ldnull", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldnull(p: Emitter) = p >> emit OpCodes.Ldnull
    [<CustomOperation("newobj", MaintainsVariableSpaceUsingBind = true)>]
    member __.Newobj(p: Emitter, ci) = p >> create ci
    [<CustomOperation("dup", MaintainsVariableSpaceUsingBind = true)>]
    member __.Dup(p: Emitter) = p >> emit OpCodes.Dup
    [<CustomOperation("newobj_expr", MaintainsVariableSpaceUsingBind = true)>]
    member __.NewobjExpr(p: Emitter, e) = p >> create (!!@ e)
    [<CustomOperation("ldfld", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldfld(p: Emitter, f) = p >> emitfld OpCodes.Ldfld f
    [<CustomOperation("ldlen", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldlen(p: Emitter) = p >> emit OpCodes.Ldlen
    [<CustomOperation("conv_i4", MaintainsVariableSpaceUsingBind = true)>]
    member __.ConvInt4(p: Emitter) = p >> emit OpCodes.Conv_I4
    [<CustomOperation("ldelem_ref", MaintainsVariableSpaceUsingBind = true)>]
    member __.LdelemRef(p: Emitter) = p >> emit OpCodes.Ldelem_Ref
    [<CustomOperation("ldelem", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldelem(p: Emitter, t) = p >> emittyp OpCodes.Ldelem t
    [<CustomOperation("ldloca", MaintainsVariableSpaceUsingBind = true)>]
    member __.Ldloca(p: Emitter, v) = p >> emitvar OpCodes.Ldloca v
    [<CustomOperation("initobj", MaintainsVariableSpaceUsingBind = true)>]
    member __.Initobj(p: Emitter, t) = p >> emittyp OpCodes.Initobj t

let emit' = EmitBuilder()

let (|List1|) = function [a] -> (a) | _ -> failwith "invalid list"
let (|List2|) = function [a; b] -> (a, b) | _ -> failwith "invalid list"
let (|List3|) = function [a; b; c] -> (a, b, c) | _ -> failwith "invalid list"
let (|List4|) = function [a; b; c; d] -> (a, b, c, d) | _ -> failwith "invalid list"

type Serialization =
    { Root: MethodInfo
      Content: MethodInfo }
    with
        static member Create (typ: Type): Serialization =
            { Root = DynamicMethod(sprintf "%s_Serialize" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |], true)
              Content = DynamicMethod(sprintf "%s_SerializeContent" typ.FullName, null, [| typeof<XmlWriter>; typeof<obj>; typeof<SerializerContext> |], true) }

type Deserialization =
    { Root: MethodInfo
      Content: MethodInfo
      MatchType: MethodInfo }
    with
        static member Create (typ: Type): Deserialization =
            { Root = DynamicMethod(sprintf "%s_Deserialize" typ.FullName, typeof<obj>, [| typeof<XmlReader>; typeof<SerializerContext> |], true)
              Content = DynamicMethod(sprintf "%s_DeserializeContent" typ.FullName, null, [| typeof<XmlReader>; typeof<obj>; typeof<int>; typeof<SerializerContext> |], true)
              MatchType = DynamicMethod(sprintf "%s_MatchType" typ.FullName, typeof<bool>, [| typeof<XmlReader> |], true) }

type TypeMap =
    { Type: Type
      Name: string
      Namespace: string option
      Layout: LayoutKind option
      IsAnonymous: bool
      DeserializeDelegate: Lazy<DeserializerDelegate>
      Deserialization: Deserialization
      SerializeDelegate: Lazy<SerializerDelegate>
      Serialization: Serialization
      CanHaveNullAsValue: bool
      BaseType: TypeMap option
      mutable IsComplete: bool }
    member this.Serialize(writer: XmlWriter, value: obj, context: SerializerContext) =
        this.SerializeDelegate.Value.Invoke(writer, value, context)
    member this.Deserialize(reader: XmlReader, context: SerializerContext) =
        this.DeserializeDelegate.Value.Invoke(reader, context)
    member this.FullName =
        match this.Namespace with
        | Some(ns) -> sprintf "%s:%s" ns this.Name
        | None -> this.Name
    static member Create(typ: Type, deserialization, serialization, baseType) =
        let attr = typ.GetCustomAttribute<XRoadTypeAttribute>() |> Option.ofObj
        let layout = attr |> Option.map (fun attr -> attr.Layout)
        { Type = typ
          Name = attr |> Option.fold (fun name attr -> match attr.Name with null | "" -> name | x -> x) typ.Name
          Namespace = attr |> Option.bind (fun attr -> match attr.Namespace with null | "" -> None | x -> Some(x))
          Layout = layout
          IsAnonymous = attr |> Option.map (fun attr -> attr.IsAnonymous) |> Option.defaultValue false
          Deserialization = deserialization
          DeserializeDelegate = lazy (deserialization.Root.CreateDelegate(typeof<DeserializerDelegate>) |> unbox)
          Serialization = serialization
          SerializeDelegate = lazy (serialization.Root.CreateDelegate(typeof<SerializerDelegate>) |> unbox)
          CanHaveNullAsValue = (not (Nullable.GetUnderlyingType(typ) |> isNull)) || (typ.IsClass && layout <> Some(LayoutKind.Choice))
          BaseType = baseType
          IsComplete = false }

type ContentWrapper =
    | Choice of TypeMap * FieldInfo * FieldInfo * int * MethodInfo
    | Method of MethodInfo * int32
    | Type of TypeMap
    member this.Name
        with get () =
            match this with Choice (tm,_,_,_,_) -> tm.Type.FullName | Method (mi, _) -> sprintf "%s.%s" mi.DeclaringType.FullName mi.Name | Type tm -> tm.FullName
    member this.Description =
        match this with
        | Choice _ -> sprintf "choice `%s`" this.Name
        | Method _ -> sprintf "operation `%s` wrapper element" this.Name
        | Type _ -> sprintf "type `%s`" this.Name

type PropertyMap =
    { TypeMap: TypeMap
      SimpleTypeName: XmlQualifiedName option
      Element: (XName * bool * bool) option
      Wrapper: ContentWrapper
      GetMethod: MethodInfo option
      SetMethod: MethodInfo option
      HasValueMethod: MethodInfo option }
    member this.HasOptionalElement with get() = match this.Element with Some(_, _, true) -> true | _ -> false

type ArrayMap =
    { Type: Type
      Element: (XName * bool * bool) option
      ItemTypeMap: TypeMap
      ItemElement: (XName * bool * bool) option
      ItemSimpleTypeName: XmlQualifiedName option
      Wrapper: ContentWrapper
      GetMethod: MethodInfo option
      SetMethod: MethodInfo option
      HasValueMethod: MethodInfo option }
    member this.GetItemPropertyMap() =
        { TypeMap = this.ItemTypeMap
          SimpleTypeName = this.ItemSimpleTypeName
          Element = this.ItemElement
          Wrapper = this.Wrapper
          GetMethod = None
          SetMethod = None
          HasValueMethod = None }
    member this.HasOptionalElement with get() = match this.Element with Some(_, _, true) -> true | _ -> false

type Property =
    | Individual of PropertyMap
    | Array of ArrayMap
    member this.Element with get() = this |> function Individual x -> x.Element | Array x -> x.Element
    member this.Wrapper with get() = this |> function Individual x -> x.Wrapper | Array x -> x.Wrapper
    member this.Type with get() = this |> function Individual x -> x.TypeMap.Type | Array x -> x.Type
    member this.GetMethod with get() = this |> function Individual x -> x.GetMethod | Array x -> x.GetMethod
    member this.SetMethod with get() = this |> function Individual x -> x.SetMethod | Array x -> x.SetMethod
    member this.PropertyName
        with get() =
            match this with
            | Individual { Element = Some(name,_,_) }
            | Array { Element = Some(name,_,_) }
            | Array { Element = None; ItemElement = Some(name,_,_) } -> Some(name)
            | _ -> None
    member this.SimpleTypeName
        with get() =
            match this with
            | Individual(x) -> x.SimpleTypeName
            | Array(_) -> Some(XmlQualifiedName("Array", XmlNamespace.SoapEnc))
    member this.HasValueMethod with get() = match this with | Individual(x) -> x.HasValueMethod | Array(x) -> x.HasValueMethod
    member this.HasOptionalElement with get() = match this with Individual(x) -> x.HasOptionalElement | Array(x) -> x.HasOptionalElement

let firstRequired (properties: Property list) =
    properties
    |> List.tryPick (fun p -> match p.Element with Some(_,_,false) -> Some(p) | _ -> None)

module internal MarkerTypes =
    type TokenString = class end
    type SwaRefAttachment = class end
    type XopAttachment = class end
    
let getMarkedType typ =
    if typ = typeof<MarkerTypes.TokenString> then typeof<string>
    elif typ = typeof<MarkerTypes.SwaRefAttachment> then typeof<BinaryContent>
    elif typ = typeof<MarkerTypes.XopAttachment> then typeof<BinaryContent>
    else typ

let getMarkerType typeHint typ =
    match typeHint with
    | TypeHint.Token -> typeof<MarkerTypes.TokenString>
    | TypeHint.SwaRef -> typeof<MarkerTypes.SwaRefAttachment>
    | TypeHint.Xop -> typeof<MarkerTypes.XopAttachment>
    | _ -> typ

let (|Serializable|NotSerializable|) (typ: Type) =
    match typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | null -> NotSerializable
    | attr -> Serializable(attr)

let safe (name: XName) = if name.NamespaceName = "" then name.LocalName else sprintf "%s:%s" name.NamespaceName name.LocalName

type PropertyInput = ContentWrapper * string * Type * bool * MethodInfo option * MethodInfo option * MethodInfo option * XRoadElementAttribute * XRoadCollectionAttribute option

let getContentOfType (typeMap: TypeMap) : PropertyInput list =
    typeMap.Type.GetProperties(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly)
    |> List.ofArray
    |> List.sortBy (fun p -> p.MetadataToken)
    |> List.choose (fun p -> p.GetCustomAttribute<XRoadElementAttribute>() |> Option.ofObj |> Option.map (fun x -> (p, x)))
    |> List.map
        (fun (p, attr) ->
            let propertyType, isOptionalType, hasValueMethod =
                if p.PropertyType.IsGenericType && p.PropertyType.GetGenericTypeDefinition() = typedefof<Optional.Option<_>> then
                    p.PropertyType.GenericTypeArguments.[0], true, Some(p.PropertyType.GetProperty("HasValue").GetGetMethod())
                else p.PropertyType, false, None
            Type typeMap, p.Name, propertyType, isOptionalType, hasValueMethod, Some(p.GetGetMethod()), Some(p.GetSetMethod(true)), attr, p.GetCustomAttribute<XRoadCollectionAttribute>() |> Option.ofObj)

let getContentOfMethod (mi: MethodInfo) : PropertyInput list =
    mi.GetParameters()
    |> Seq.choose (fun p -> p.GetCustomAttribute<XRoadElementAttribute>() |> Option.ofObj |> Option.map (fun x -> (p, x)))
    |> Seq.mapi (fun i (p, attr) ->
        let parameterType, isOptionalType, hasValueMethod =
            if p.ParameterType.IsGenericType && p.ParameterType.GetGenericTypeDefinition() = typedefof<Optional.Option<_>> then
                p.ParameterType.GenericTypeArguments.[0], true, Some(p.ParameterType.GetProperty("HasValue").GetGetMethod())
            else p.ParameterType, false, None
        Method(mi, i), p.Name, parameterType, isOptionalType, hasValueMethod, None, None, attr, p.GetCustomAttribute<XRoadCollectionAttribute>() |> Option.ofObj)
    |> Seq.toList

let getContentOfChoice (choiceMap: TypeMap) : PropertyInput list =
    let choiceType = choiceMap.Type
    let idField =
        match choiceType.GetField("__id", BindingFlags.Instance ||| BindingFlags.NonPublic) with
        | null -> choiceType.GetField("__id@", BindingFlags.Instance ||| BindingFlags.NonPublic)
        | x -> x
    let valueField =
        match choiceType.GetField("__value", BindingFlags.Instance ||| BindingFlags.NonPublic) with
        | null -> choiceType.GetField("__value@", BindingFlags.Instance ||| BindingFlags.NonPublic)
        | x -> x
    choiceType.GetCustomAttributes<XRoadElementAttribute>()
    |> Seq.map
        (fun attr ->
            let (typ, mi) =
                let methodName = sprintf "New%s%s" (if Char.IsLower(attr.Name.[0]) then "_" else "") attr.Name
                match choiceType.GetMethod(methodName, BindingFlags.Public ||| BindingFlags.Static) with
                | null -> failwithf "Type `%s` should define public static method `%s`." choiceType.FullName methodName
                | mi -> match mi.GetParameters() with
                        | [| pi |] -> (pi.ParameterType, mi)
                        | _ -> failwithf "Type `%s` method `New%s` should have exactly one argument." choiceType.FullName attr.Name
            let parameterType, isOptionalType, hasValueMethod =
                if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Optional.Option<_>> then
                    typ.GenericTypeArguments.[0], true, Some(typ.GetProperty("HasValue").GetGetMethod())
                else typ, false, None
            let collectionAttr = choiceType.GetCustomAttributes<XRoadCollectionAttribute>() |> Seq.tryFind (fun a -> a.Id = attr.Id)
            Choice (choiceMap, idField, valueField, attr.Id, mi), attr.Name, parameterType, isOptionalType, hasValueMethod, None, None, attr, collectionAttr)
    |> Seq.toList

module EmitSerialization =
    /// Check if values type matches expected type.
    let private emitValueTypeTest (expectedType: Type) = emit' {
        ldarg_1
        call_expr <@ (null: obj).GetType() @>
        callvirt_expr <@ (null: Type).FullName @>
        ldstr expectedType.FullName
        string_equals
    }

    /// Write type attribute according to TypeMap.
    let emitTypeAttribute (typeName: string) (typeNamespace: string option) = emit' {
        ldarg_0
        ldstr "type"
        ldstr XmlNamespace.Xsi
        callvirt_expr <@ (null: XmlWriter).WriteStartAttribute("", "") @>
        nop
        ldarg_0
        ldstr typeName
        merge (
            match typeNamespace with
            | Some(ns) ->
                emit' {
                    ldstr ns
                    callvirt_expr <@ (null: XmlWriter).WriteQualifiedName("", "") @>
                }
            | None ->
                emit' {
                    callvirt_expr <@ (null: XmlWriter).WriteString("") @>
                }
        )
        nop
        ldarg_0
        callvirt_expr <@ (null: XmlWriter).WriteEndAttribute() @>
        nop
    }

    /// Emit type (and its base types) content serialization.
    let rec private emitContentSerialization (typeMap: TypeMap) = emit' {
        merge (match typeMap.BaseType with Some(tm) -> emitContentSerialization tm | None -> id)
        ldarg_0
        ldarg_1
        ldarg_2
        call typeMap.Serialization.Content
        nop
    }

    /// Emit abstract type test and exception.
    let private emitAbstractTypeException (typeMap: TypeMap) = emit' {
        ldstr (sprintf "Cannot serialize abstract type `%s`." typeMap.FullName)
        newobj_expr <@ Exception("") @>
        throw
    }

    /// Emit whole contents of TypeMap serialization.
    let private emitBodySerialization addType (typeMap: TypeMap) =
        if typeMap.Type.IsAbstract then emitAbstractTypeException typeMap else emit' {
            merge (if addType then emitTypeAttribute typeMap.Name typeMap.Namespace else id)
             // TODO : Attributes
            merge (emitContentSerialization typeMap)
        }

    /// Emit serialization taking into consideration if actual type matches subtype or not.
    let rec private emitTypeHierarchySerialization (markReturn: Label) isEncoded (subTypes: TypeMap list) typeMap =
        match subTypes with
        | [] ->
            emitBodySerialization isEncoded typeMap
        | subType::other ->
            emit' {
                define_label (fun markNext -> emit' {
                    // Check if type matches current TypeMap.
                    merge (emitValueTypeTest subType.Type)
                    brfalse markNext
                    merge (emitBodySerialization true subType)
                    br markReturn
                    set_marker markNext
                })
                nop
                merge (emitTypeHierarchySerialization markReturn isEncoded other typeMap)
            }

    let emitNilAttribute (markReturn: Label) = emit' {
        define_label (fun markNotNull -> emit' {
            ldnull
            ceq
            brfalse markNotNull
            ldarg_0
            ldstr "nil"
            ldstr XmlNamespace.Xsi
            ldstr "true"
            callvirt_expr <@ (null: XmlWriter).WriteAttributeString("", "", "") @>
            nop
            br markReturn
            set_marker markNotNull
        })
        nop
    }

    /// Emit root type serialization logic for given TypeMap.
    let emitRootSerializerMethod isEncoded (subTypes: TypeMap list) (typeMap: TypeMap) = emit' {
        define_label (fun markReturn -> emit' {
            // When value is `null`, write `xsi:nil` attribute and return.
            ldarg_1
            merge (emitNilAttribute markReturn)
            // Serialize value according to its type.
            merge (emitTypeHierarchySerialization markReturn isEncoded subTypes typeMap)
            set_marker markReturn
        })
        // Return
        ret
    }

    /// Provides value for array item at current index.
    let private emitArrayItemValue (array: LocalBuilder) (index: LocalBuilder) (typ: Type) = emit' {
        ldloc array
        ldloc index
        ldelem typ
        merge (if typ.IsValueType then emit' { box typ } else id)
    }

    /// Emit validation for not nullable types to have value specified.
    let private emitNotNullableCheck (name: string) emitValue property =
        match property with
        | Array _
        | Individual { TypeMap = { CanHaveNullAsValue = true } } ->
            emit' {
                define_label (fun markSuccess -> emit' {
                    // Check if value is null.
                    merge (emitValue property.Type)
                    ldnull
                    ceq
                    brfalse markSuccess
                    // Not nullable shouldn't have null as value, so throw exception.
                    ldstr (sprintf "Not nullable property `%s` of type `%s` has null value." name property.Wrapper.Name)
                    newobj_expr <@ Exception("") @>
                    throw
                    set_marker markSuccess
                })
                nop
            }
        | _ -> id

    let emitPropertyWrapperSerialization (property: Property) =
        match property.Wrapper with
        | Choice (_,_,fld,_,_) ->
            let ty = property.HasValueMethod |> Option.map (fun m -> m.DeclaringType) |> Option.defaultWith (fun _ -> property.Type)
            emit' {
                ldarg_1
                castclass fld.DeclaringType
                ldfld fld
                merge (if property.Type.IsValueType then emit' { unbox ty } else emit' { castclass ty })
            }
        | Method (_, i) ->
            let ty = property.HasValueMethod |> Option.map (fun m -> m.DeclaringType) |> Option.defaultWith (fun _ -> property.Type)
            emit' {
                ldarg_1
                ldc_i4 i
                ldelem_ref
                merge (if ty.IsValueType then emit' { unbox ty } else emit' { castclass ty })
            }
        | Type tm ->
            emit' {
                ldarg_1
                castclass tm.Type
                callvirt property.GetMethod.Value
            }

    let emitOptionalFieldSerialization (property: Property) emitContent =
        if not property.HasOptionalElement then emitContent else
        emit' {
            merge (emitPropertyWrapperSerialization property)
            declare_variable (lazy (declareLocal property.HasValueMethod.Value.DeclaringType)) (fun optionalType -> emit' {
                stloc optionalType
                ldloca optionalType
                call property.HasValueMethod.Value
                define_label (fun endContentLabel -> emit' {
                    brfalse endContentLabel
                    nop
                    merge emitContent
                    set_marker endContentLabel
                })
                nop
            })
        }

    /// Emit single property content serialization.
    let rec emitPropertyContentSerialization emitValue isEncoded (property: Property) : ILGenerator -> ILGenerator =
        // Write start element of the propery if its not merged with content.
        let writeStartElement =
            match property.Element with
            | Some(name, isNullable, _) ->
                emit' {
                    ldarg_0
                    ldstr name.LocalName
                    ldstr name.NamespaceName
                    callvirt_expr <@ (null: XmlWriter).WriteStartElement("", "") @>
                    nop
                    merge (if isNullable then id else emitNotNullableCheck name.LocalName emitValue property)
                    merge (
                        match isEncoded, property.SimpleTypeName with
                        | true, Some(typeName) -> emitTypeAttribute typeName.Name (Some(typeName.Namespace))
                        | _ -> id
                    )
                }
            | None -> id

        // Serialize property content value according to its TypeMap.
        let writePropertyContent = emit' {
            define_label (fun markReturn -> emit' {
                merge(
                    match property with
                    | Individual propertyMap ->
                        emit' {
                            ldarg_0
                            merge (emitValue propertyMap.TypeMap.Type)
                            ldarg_2
                            call propertyMap.TypeMap.Serialization.Root
                            nop
                        }
                    | Array arrayMap ->
                        let itemPropertyMap = Individual (arrayMap.GetItemPropertyMap())
                        emit' {
                            merge (emitValue arrayMap.Type)
                            merge (
                                match property.Element with
                                | Some(_) ->
                                    emitNilAttribute markReturn
                                | None -> 
                                    emit' {
                                        ldnull
                                        ceq
                                        brtrue markReturn
                                    }
                            )
                            merge (emitValue arrayMap.Type)
                            declare_variable (lazy (declareLocal arrayMap.Type)) (fun arr -> emit' {
                                stloc arr
                                declare_variable (lazy declareLocalOf<int>) (fun i -> emit' {
                                    ldc_i4_0
                                    stloc i
                                    define_labels 2 (fun (List2(markLoopCondition, markLoopStart)) -> emit' {
                                        br markLoopCondition
                                        set_marker markLoopStart
                                        nop
                                        merge (emitPropertyContentSerialization (emitArrayItemValue arr i) isEncoded itemPropertyMap)
                                        ldloc i
                                        ldc_i4_1
                                        add
                                        stloc i
                                        set_marker markLoopCondition
                                        ldloc i
                                        ldloc arr
                                        ldlen
                                        conv_i4
                                        clt
                                        brtrue markLoopStart
                                    })
                                })
                            })
                        }
                )
                set_marker markReturn
                nop
            })
        }

        // Write end element if required.
        let writeEndElement =
            match property.Element with
            | Some(_) ->
                emit' {
                    ldarg_0
                    callvirt_expr <@ (null: XmlWriter).WriteEndElement() @>
                    nop
                }
            | None -> id

        writeStartElement >> writePropertyContent >> writeEndElement

    /// Unbox property value into correct type.
    let emitPropertyValue (property: Property) (typ: Type) = emit' {
        merge (emitPropertyWrapperSerialization property)
        merge (
            if not property.HasOptionalElement then id else emit' {
                declare_variable (lazy (declareLocal property.HasValueMethod.Value.DeclaringType)) (fun opt -> emit' {
                    stloc opt
                    ldloca opt
                    merge (
                        if not typ.IsValueType then emit' { ldnull } else emit' {
                            declare_variable (lazy (declareLocal typ)) (fun temp -> emit' {
                                ldloca temp
                                initobj typ
                                ldloc temp
                            })
                        }
                    )
                    call (property.HasValueMethod.Value.DeclaringType.GetMethod("ValueOr", [| typ |]))
                })
            }
        )
        merge (if typ.IsValueType then emit' { box typ } else id)
    }

    /// Emit IL which serializes each property value into corresponding xml fragment.
    let emitContentSerializerMethod isEncoded (properties: Property list) =
        properties
        |> List.map (fun property -> emitOptionalFieldSerialization property (emitPropertyContentSerialization (emitPropertyValue property) isEncoded property))
        |> (fun items -> if items.IsEmpty then id else items |> List.reduce ((>>)))

module EmitDeserialization =
    let emitDebug arg = emit' {
        ldstr (sprintf "%s: {3}: <{1}:{0}> [{2}]" arg)
        ldarg_0
        callvirt_expr <@ (null: XmlReader).LocalName @>
        ldarg_0
        callvirt_expr <@ (null: XmlReader).NamespaceURI @>
        ldarg_0
        callvirt_expr <@ (null: XmlReader).Depth @>
        box typeof<int>
        ldarg_0
        callvirt_expr <@ (null: XmlReader).NodeType @>
        box typeof<int>
        call_expr <@ Console.WriteLine("", "", "", "", "") @>
    }

    /// Check if current element has `xsi:nil` attribute present.
    let private emitNullCheck = emit' {
        ldarg_0
        call_expr <@ (null: XmlReader).ReadXsiNullAttribute() @>
    }

    /// Reads type attribute value and stores name and namespace in variables.
    let private emitTypeAttributeRead = emit' {
        ldarg_0
        call_expr <@ (null: XmlReader).ReadXsiTypeAttribute() @>
    }

    /// Check if value type matches expected type.
    let private emitValueTypeTest isDefault (qualifiedNameVar: LocalBuilder) (typeMap: TypeMap) = emit' {
        ldarg_0
        ldloc qualifiedNameVar
        ldstr typeMap.Name
        ldstr (typeMap.Namespace |> Option.defaultValue "")
        merge (if typeMap.IsAnonymous then (emit' { ldc_i4_1 }) else (emit' { ldc_i4_0 }))
        merge (if isDefault then (emit' { ldc_i4_1 }) else (emit' { ldc_i4_0 }))
        call_expr <@ (null: XmlReader).IsQualifiedTypeName(null, "", "", false, false) @>
    }
    
    let emitXmlReaderRead = emit' {
        ldarg_0
        callvirt_expr <@ (null: XmlReader).Read() @>
        pop
    }

    /// Emit type (and its base types) content deserialization.
    let rec private emitContentDeserialization hasInlineContent (instance: LocalBuilder, depthVar: LocalBuilder) (typeMap: TypeMap) = emit' {
        merge (
            if hasInlineContent then id else
            emit' {
                ldarg_0
                callvirt_expr <@ (null: XmlReader).IsEmptyElement @>
                define_label (fun skipRead -> emit' {
                    brtrue skipRead
                    merge emitXmlReaderRead
                    set_marker skipRead
                })
            }
        )
        ldarg_0
        ldloc instance
        ldloc depthVar
        merge (
            if hasInlineContent then id else emit' {
                ldc_i4_1
                add
            }
        )
        ldarg_1
        call typeMap.Deserialization.Content
    }

    /// Emit abstract type test and exception.
    let private emitAbstractTypeException (typeMap: TypeMap) = emit' {
        ldstr (sprintf "Cannot deserialize abstract type `%s`." typeMap.FullName)
        newobj_expr <@ Exception("") @>
        throw
    }

    let emitWrongElementException expectedValue wrapper =
        let wrapperName =
            match wrapper with
            | Choice _ -> sprintf "choice `%s`" wrapper.Name
            | Method _ -> sprintf "operation `%s` wrapper element" wrapper.Name
            | Type _ -> sprintf "type `%s`" wrapper.Name
        emit' {
            ldstr (sprintf "Element `%s` was expected in subsequence of %s, but element `{0}` was found instead." expectedValue wrapperName)
            ldarg_0
            callvirt_expr <@ (null: XmlReader).LocalName @>
            call_expr <@ String.Format("", "") @>
            newobj_expr <@ Exception("") @>
            throw
        }

    /// Emit whole contents of TypeMap deserialization.
    let private emitBodyDeserialization hasInlineContent (depthVar: LocalBuilder) (typeMap: TypeMap) =
        if typeMap.Type.IsAbstract then emitAbstractTypeException typeMap else
        emit' {
            newobj (typeMap.Type.GetConstructor(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public, null, [| |], [| |]))
            declare_variable (lazy (declareLocal typeMap.Type)) (fun instance -> emit' {
                stloc instance
                // TODO : Attributes
                merge (emitContentDeserialization hasInlineContent (instance, depthVar) typeMap)
                ldarg_0
                ldstr typeMap.Name
                ldstr (typeMap.Namespace |> Option.defaultValue "")
                ldloc depthVar
                ldc_i4_0
                call_expr <@ (null: XmlReader).ReadToNextElement("", "", 0, false) @>
                ldloc instance
            })
        }

    /// Emit deserialization taking into consideration if actual type matches subtype or not.
    let rec private emitTypeHierarchyDeserialization hasInlineContent (markReturn: Label) (depthVar: LocalBuilder) (subTypes: TypeMap list) qualifiedName typeMap =
        match subTypes with
        | [] ->
            let context = if typeMap.IsAnonymous then "anonymous type" else sprintf "type `%s`" (XName.Get(typeMap.Name, typeMap.Namespace |> Option.defaultValue "") |> safe)
            emit' {
                define_label (fun errorLabel -> emit' {
                    merge (emitValueTypeTest true qualifiedName typeMap)
                    brfalse errorLabel
                    merge (emitBodyDeserialization hasInlineContent depthVar typeMap)
                    br markReturn
                    set_marker errorLabel
                })
                ldstr (sprintf "Unexpected type value: using type `{0}` is not allowed in the context of %s." context)
                ldloc qualifiedName
                call_expr <@ String.Format("", "") @>
                newobj_expr <@ Exception("") @>
                throw
            }
        | subType::other ->
            emit' {
                define_label (fun markNext -> emit' {
                    merge (emitValueTypeTest false qualifiedName subType)
                    brfalse markNext
                    merge (emitBodyDeserialization hasInlineContent depthVar subType)
                    br markReturn
                    set_marker markNext
                })
                nop
                merge (emitTypeHierarchyDeserialization hasInlineContent markReturn depthVar other qualifiedName typeMap)
            }

    let emitRootDeserializerMethod hasInlineContent (subTypes: TypeMap list) (typeMap: TypeMap) = emit' {
        declare_variable (lazy declareLocalOf<int>) (fun depthVar -> emit' {
            ldarg_0
            callvirt_expr <@ (null: XmlReader).Depth @>
            stloc depthVar
            define_label (fun markReturn -> emit' {
                merge emitNullCheck
                define_label (fun label -> emit' {
                    brfalse label
                    ldarg_0
                    ldarg_0
                    callvirt_expr <@ (null: XmlReader).LocalName @>
                    ldarg_0
                    callvirt_expr <@ (null: XmlReader).NamespaceURI @>
                    ldarg_0
                    callvirt_expr <@ (null: XmlReader).Depth @>
                    ldc_i4_0
                    call_expr <@ (null: XmlReader).ReadToNextElement("", "", 0, false) @>
                    ldnull
                    br markReturn
                    set_marker label
                })
                declare_variable (lazy declareLocalOf<XmlQualifiedName>) (fun qualifiedName -> emit' {
                    merge emitTypeAttributeRead
                    stloc qualifiedName
                    merge (emitTypeHierarchyDeserialization hasInlineContent markReturn depthVar subTypes qualifiedName typeMap)
                })
                set_marker markReturn
            })
        })
        ret
    }

    let emitPropertyValueDeserialization (isContent: bool) (typeMap: TypeMap) = emit' {
        ldarg_0
        merge (if isContent then (emit' { ldarg_3 }) else (emit' { ldarg_1 }))
        call typeMap.Deserialization.Root
        merge (if typeMap.Type.IsValueType then (emit' { unbox typeMap.Type }) else (emit' { castclass typeMap.Type }))
    }

    let emitPropertyWrapperDeserialization (wrapper: ContentWrapper) =
        match wrapper with
        | Choice _ -> id
        | Method _ -> failwith "Method signature is not deserialzable"
        | Type tm -> emit' {
            ldarg_1
            castclass tm.Type
        }

    let optionalSomeMethod =
        typeof<Optional.Option>.GetMethods()
        |> Array.filter (fun m -> m.Name = "Some" && m.GetGenericArguments().Length = 1)
        |> Array.exactlyOne

    let emitIndividualPropertyDeserialization isContent (propertyMap: PropertyMap) = emit' {
        merge (emitPropertyValueDeserialization isContent propertyMap.TypeMap)
        declare_variable (lazy (declareLocal propertyMap.TypeMap.Type)) (fun x -> emit' {
            stloc x
            merge (emitPropertyWrapperDeserialization propertyMap.Wrapper)
            ldloc x
            merge (
                if propertyMap.HasOptionalElement then (emit' {
                    call (optionalSomeMethod.MakeGenericMethod([| propertyMap.TypeMap.Type |]))
                }) else id
            )
        })
    }

    let emitArrayItemDeserialization isContent (arrayMap: ArrayMap, listInstance: LocalBuilder, markEnd: Label, stopIfWrongElement) =
        match arrayMap.ItemElement with
        | Some(name, _, _) ->
            emit' {
                ldarg_0
                ldstr name.LocalName
                ldstr name.NamespaceName
                call_expr <@ (null: XmlReader).IsMatchingElement("", "") @>
                define_label (fun deserializeLabel -> emit' {
                    brtrue deserializeLabel
                    merge (
                        if stopIfWrongElement then (emit' { br markEnd }) else (emit' {
                            ldstr "Unexpected element: found `{0}`, but was expecting to find `{1}`."
                            ldarg_0
                            callvirt_expr <@ (null: XmlReader).LocalName @>
                            ldstr (safe name)
                            call_expr <@ String.Format("", "", "") @>
                            newobj_expr <@ Exception("") @>
                            throw
                        })
                    )
                    set_marker deserializeLabel
                    ldloc listInstance
                    merge (emitPropertyValueDeserialization isContent arrayMap.ItemTypeMap)
                    callvirt (listInstance.LocalType.GetMethod("Add", [| arrayMap.ItemTypeMap.Type |]))
                })
            }
        | None -> id

    /// Emits array type deserialization logic.
    let emitArrayPropertyDeserialization isContent arrDepthVar (arrayMap: ArrayMap) =
        let listType = typedefof<ResizeArray<_>>.MakeGenericType(arrayMap.ItemTypeMap.Type)
        emit' {
            ldloc arrDepthVar
            declare_variable (lazy declareLocalOf<int>) (fun depthVar -> emit' {
                stloc depthVar
                define_labels 3 (fun (List3 (markArrayNull, markArrayEnd, markLoopStart)) -> emit' {
                    merge (
                        if arrayMap.Element.IsSome then (emit' {
                            merge emitNullCheck
                            define_label (fun label -> emit' {
                                brfalse label
                                ldarg_0
                                ldarg_0
                                callvirt_expr <@ (null: XmlReader).LocalName @>
                                ldarg_0
                                callvirt_expr <@ (null: XmlReader).NamespaceURI @>
                                ldarg_0
                                callvirt_expr <@ (null: XmlReader).Depth @>
                                ldc_i4_0
                                call_expr <@ (null: XmlReader).ReadToNextElement("", "", 0, false) @>
                                ldnull
                                br markArrayNull
                                set_marker label
                            })
                            ldloc depthVar
                            ldc_i4_1
                            add
                            stloc depthVar
                        }) else id
                    )
                    declare_variable (lazy (declareLocal listType)) (fun listInstance -> emit'{
                        newobj (listType.GetConstructor([||]))
                        stloc listInstance
                        merge (
                            if arrayMap.Element.IsSome then (emit' {
                                ldarg_0
                                callvirt_expr <@ (null: XmlReader).IsEmptyElement @>
                                brtrue markArrayEnd
                                merge emitXmlReaderRead
                            }) else id
                        )
                        set_marker markLoopStart
                        ldarg_0
                        ldloc depthVar
                        call_expr <@ (null: XmlReader).FindNextStartElement(0) @>
                        brfalse markArrayEnd
                        merge (emitArrayItemDeserialization isContent (arrayMap, listInstance, markArrayEnd, arrayMap.Element.IsNone))
                        br markLoopStart
                        set_marker markArrayEnd
                        merge (
                            match arrayMap.Element with
                            | Some(name, _, _) ->
                                emit' {
                                    ldarg_0
                                    ldstr name.LocalName
                                    ldstr name.NamespaceName
                                    ldloc arrDepthVar
                                    ldc_i4_0
                                    call_expr <@ (null: XmlReader).ReadToNextElement("", "", 0, false) @>
                                }
                            | None -> id
                        )
                        declare_variable (lazy (declareLocal arrayMap.Type)) (fun instance -> emit' {
                            ldloc listInstance
                            callvirt (listType.GetMethod("ToArray", [| |]))
                            set_marker markArrayNull
                            stloc instance
                            merge (emitPropertyWrapperDeserialization arrayMap.Wrapper)
                            ldloc instance
                            merge (
                                if arrayMap.HasOptionalElement then (emit' {
                                    call (optionalSomeMethod.MakeGenericMethod([| arrayMap.Type |]))
                                }) else id
                            )
                        })
                    })
                })
            })
        }

    let emitMatchType property =
        match property with
        | Some(Individual { TypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap })
        | Some(Array { Element = None; ItemTypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap }) ->
            emit' {
                ldarg_0
                call typeMap.Deserialization.MatchType
            }
        | None
        | Some(Individual { Element = None })
        | Some(Array { Element = None; ItemElement = None }) ->
            emit' {
                ldc_i4_0
            }
        | Some(Individual { Element = Some(name,_,_) })
        | Some(Array { Element = Some(name,_,_) })
        | Some(Array { Element = None; ItemElement = Some(name,_,_) }) ->
            emit' {
                ldarg_0
                ldstr name.LocalName
                ldstr name.NamespaceName
                call_expr <@ (null: XmlReader).IsMatchingElement("", "") @>
            }

    let emitPropertyDeserialization depthVar property = emit' {
        merge (
            match property with
            | Individual propertyMap -> emitIndividualPropertyDeserialization true propertyMap
            | Array arrayMap -> emitArrayPropertyDeserialization true depthVar arrayMap
        )
        callvirt property.SetMethod.Value
    }

    let rec emitSequenceDeserialization (startLabel: Label) (returnLabel: Label) (depthVar: LocalBuilder) (properties: Property list) =
        let nextRequired = properties |> firstRequired
        match properties with
        | [] ->
            emit' {
                set_marker startLabel
                nop
            }
        | property::properties ->
            emit' {
                set_marker startLabel

                ldarg_0
                ldloc depthVar
                call_expr <@ (null: XmlReader).FindNextStartElement(0) @>

                merge (
                    match nextRequired with
                    | Some(rp) ->
                        emit' {
                            define_label (fun found -> emit' {
                                brtrue found
                                merge (emitWrongElementException (safe (rp.PropertyName.Value)) rp.Wrapper)
                                set_marker found
                            })
                        }
                    | None -> emit' { brfalse returnLabel }
                )

                define_labels 2 (fun (List2(markNext, markDeserialize)) -> emit' {
                    merge (
                        match property with
                        | Individual { Element = Some(name,_,isOptional) }
                        | Array { Element = Some(name,_,isOptional) } ->
                            emit' {
                                ldarg_0
                                ldstr name.LocalName
                                ldstr name.NamespaceName
                                call_expr <@ (null: XmlReader).IsMatchingElement("", "") @>
                                brtrue markDeserialize
                                merge (if isOptional then (emit' { br markNext }) else (emitWrongElementException (safe name) property.Wrapper))
                                set_marker markDeserialize
                            }
                        | _ -> id
                    )

                    merge (emitPropertyDeserialization depthVar property)
                    merge (emitSequenceDeserialization markNext returnLabel depthVar properties)
                })
            }

let (|InlineContent|_|) (properties: Property list) =
    match properties with
    | [Individual({ Element = None; TypeMap = typeMap }) as prop]
    | [Array({ Element = None; ItemElement = None; ItemTypeMap = typeMap }) as prop] ->
        match typeMap.Layout with
        | Some(LayoutKind.Choice) -> None
        | _ -> Some(prop)
    | _ -> None

let rec private createDeserializeContentMethodBody (typeMap: TypeMap) (properties: Property list) =
    emit' {
        define_label (fun returnLabel -> emit' {
            ldarg_2
            declare_variable (lazy declareLocalOf<int>) (fun depthVar -> emit' {
                stloc depthVar
                merge (
                    match properties with
                    | InlineContent(prop) ->
                        EmitDeserialization.emitPropertyDeserialization depthVar prop
                    | _ ->
                        emit' {
                            merge (
                                match typeMap.BaseType with
                                | Some(typeMap) ->
                                    emit' {
                                        ldarg_0
                                        ldarg_1
                                        ldloc depthVar
                                        ldarg_3
                                        call typeMap.Deserialization.Content
                                    }
                                | None -> id
                            )
                            define_label (fun startLabel -> emit' {
                                merge (match typeMap.Layout.Value with
                                       | LayoutKind.Sequence -> EmitDeserialization.emitSequenceDeserialization startLabel returnLabel depthVar properties
                                       | _ -> failwith "Not implemented")
                            })
                        }
                )
            })
            set_marker returnLabel
            ret
        })
    }

and createTypeSerializers isEncoded (typeMap: TypeMap) =
    let properties = getContentOfType typeMap |> getProperties (getTypeMap isEncoded)
    let directSubTypes = typeMap.Type |> findDirectSubTypes isEncoded

    // Emit serializers
    defineMethod typeMap.Serialization.Root
        (EmitSerialization.emitRootSerializerMethod isEncoded directSubTypes typeMap)

    defineMethod typeMap.Serialization.Content (emit' {
        merge (EmitSerialization.emitContentSerializerMethod isEncoded properties)
        ret
    })

    // Emit deserializers
    defineMethod typeMap.Deserialization.Root
        (EmitDeserialization.emitRootDeserializerMethod (match properties with InlineContent _ -> true | _ -> false) directSubTypes typeMap)

    defineMethod typeMap.Deserialization.Content
        (createDeserializeContentMethodBody typeMap properties)

    match properties with
    | [Individual { Element = None }] | [Array { Element = None; ItemElement = None }] ->
        ()
    | _ ->
        defineMethod typeMap.Deserialization.MatchType (emit' {
            merge (EmitDeserialization.emitMatchType (properties |> List.tryHead))
            ret
        })

and createChoiceTypeSerializers isEncoded (properties: Property list) (choiceMap: TypeMap) =
    let genSerialization () =
        let rec generate (conditionEnd: Label) (label: Label option) (properties: Property list) =
            match properties with
            | [] -> id
            | property::other ->
                let idField, tag = match property.Wrapper with Choice(_, idField, _, id, _) -> idField, id | _ -> failwith "never"
                let nextBlock nextLabel = emit' {
                    brfalse nextLabel
                    nop
                    merge (EmitSerialization.emitOptionalFieldSerialization property (EmitSerialization.emitPropertyContentSerialization (EmitSerialization.emitPropertyValue property) isEncoded property))
                    nop
                    br conditionEnd
                    merge (generate conditionEnd (Some nextLabel) other)
                }
                emit' {
                    merge (
                        match label with
                        | Some(label) ->
                            emit' {
                                set_marker label
                                nop
                            }
                        | None -> id
                    )
                    ldarg_1
                    castclass choiceMap.Type
                    ldfld idField
                    ldc_i4 tag
                    ceq
                    merge (if other.IsEmpty then (nextBlock conditionEnd) else (emit' { define_label nextBlock })) 
                }

        defineMethod choiceMap.Serialization.Root (emit' {
            define_label (fun conditionEnd -> emit' {
                merge (generate conditionEnd None properties)
                set_marker conditionEnd
                ret
            })
        })

    let genContentDeserialization () =
        defineMethod choiceMap.Deserialization.Content (emit' { ret })

    let genDeserialization () =
        let rec generate (depthVar: LocalBuilder) (markReturn: Label) (properties: Property list) =
            match properties with
            | [] -> id
            | property::other ->
                let mi = match property.Wrapper with Choice(_, _, _, _, mi) -> mi | _ -> failwith "never"
                emit' {
                    define_label (fun label -> emit' {
                        merge (
                            match property with
                            | Individual { TypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap }
                            | Individual { Element = None; TypeMap = typeMap }
                            | Array { Element = None; ItemTypeMap = { Layout = Some(LayoutKind.Choice) } as typeMap }
                            | Array { Element = None; ItemElement = None; ItemTypeMap = typeMap } ->
                                emit' {
                                    declare_variable (lazy (declareLocal property.Type)) (fun instance -> emit' {
                                        ldarg_0
                                        ldloc depthVar
                                        call_expr <@ (null: XmlReader).FindNextStartElement(0) @>
                                        pop
                                        ldarg_0
                                        call typeMap.Deserialization.MatchType
                                        brfalse label
                                        newobj (typeMap.Type.GetConstructor([| |]))
                                        stloc instance
                                        ldarg_0
                                        ldloc instance
                                        ldloc depthVar
                                        ldarg_1
                                        call typeMap.Deserialization.Content
                                        ldloc instance
                                    })
                                }
                            | Individual { Element = Some(name,_,_) }
                            | Array { Element = Some(name,_,_) }
                            | Array { Element = None; ItemElement = Some(name,_,_) } ->
                                emit' {
                                    ldarg_0
                                    ldloc depthVar
                                    call_expr <@ (null: XmlReader).FindNextStartElement(0) @>
                                    pop
                                    ldarg_0
                                    ldstr name.LocalName
                                    ldstr name.NamespaceName
                                    call_expr <@ (null: XmlReader).IsMatchingElement("", "") @>
                                    brfalse label
                                    merge (
                                        match property with
                                        | Individual propertyMap -> EmitDeserialization.emitIndividualPropertyDeserialization false propertyMap
                                        | Array arrayMap -> EmitDeserialization.emitArrayPropertyDeserialization false depthVar arrayMap
                                    )
                                }
                        )
                        call mi
                        br markReturn
                        set_marker label
                        nop
                        merge (generate depthVar markReturn other)
                    })
                }

        defineMethod choiceMap.Deserialization.Root (emit' {
            define_label (fun markReturn -> emit' {
                declare_variable (lazy declareLocalOf<int>) (fun depthVar ->
                    let names = properties |> List.map (fun p -> p.PropertyName)
                    let errorMessage = sprintf "Invalid message: expected one of %A, but `{0}` was found instead." names 
                    emit' {
                        ldarg_0
                        callvirt_expr <@ (null: XmlReader).Depth @>
                        stloc depthVar
                        merge (generate depthVar markReturn properties)
                        ldstr errorMessage
                        ldarg_0
                        callvirt_expr <@ (null: XmlReader).LocalName @>
                        call_expr <@ String.Format("", "") @>
                        newobj_expr <@ Exception("") @>
                        throw
                    }
                )
                set_marker markReturn
            })
            ret
        })

    let genMatch () =
        defineMethod choiceMap.Deserialization.MatchType (emit' {
            define_label (fun markReturn ->
                let rec generate (properties: Property list) =
                    match properties with
                    | [] ->
                        emit' {
                            ldc_i4_0
                            br markReturn
                        }
                    | property::other ->
                        emit' {
                            merge (EmitDeserialization.emitMatchType (Some property))
                            define_label (fun label -> emit' {
                                brfalse label
                                ldc_i4_1
                                br markReturn
                                set_marker label
                            })
                            nop
                            merge (generate other)
                        }
                emit' {
                    merge (generate properties)
                    set_marker markReturn
                })
            ret
        })

    genSerialization ()
    genContentDeserialization ()
    genDeserialization ()
    genMatch ()

and private getProperties (tmf: Type -> TypeMap) (input: PropertyInput list) : Property list =
    input
    |> List.map
        (fun (wrapper, propName, propertyType, isOptionalType, hasValueMethod, getMethod, setMethod, attr, cattr) ->
            let name = match attr.Name with null | "" -> propName | name -> name
            let xname = match attr.Namespace with "" -> XName.Get(name) | ns -> XName.Get(name, ns)
            let element = if attr.MergeContent then None else Some(xname, attr.IsNullable, isOptionalType)
            if propertyType.IsArray then
                let elementType = propertyType.GetElementType()
                let itemTypeMap = elementType |> getMarkerType attr.TypeHint |> tmf
                let itemName = cattr |> Option.bind (fun a -> match a.ItemName with null | "" -> None | name -> Some(name)) |> Option.defaultWith (fun _ -> "item")
                let itemXName = cattr |> Option.bind (fun a -> match a.ItemNamespace with "" -> None | ns -> Some(XName.Get(itemName, ns))) |> Option.defaultWith (fun _ -> XName.Get(itemName))
                let itemElement = if itemTypeMap.Layout <> Some(LayoutKind.Choice)
                                  then if cattr.IsSome && cattr.Value.MergeContent then None else Some(itemXName, cattr.IsSome && cattr.Value.ItemIsNullable, true)
                                  else None
                Array { Type = propertyType
                        Element = element
                        ItemTypeMap = itemTypeMap
                        ItemElement = itemElement
                        ItemSimpleTypeName = getSystemTypeName elementType.FullName
                        Wrapper = wrapper
                        GetMethod = getMethod
                        SetMethod = setMethod
                        HasValueMethod = hasValueMethod }
            else
                let propertyTypeMap = propertyType |> getMarkerType attr.TypeHint |> tmf
                let element = if propertyTypeMap.Layout <> Some(LayoutKind.Choice) then element else None
                Individual { TypeMap = propertyTypeMap
                             SimpleTypeName = getSystemTypeName (propertyType.FullName)
                             Element = element
                             Wrapper = wrapper
                             GetMethod = getMethod
                             SetMethod = setMethod
                             HasValueMethod = hasValueMethod })

and private typeMaps = ConcurrentDictionary<Type, TypeMap>()

and private createTypeMap (isEncoded: bool) (typ: Type) =
    let addTypeMap (init: TypeMap -> unit) (typ: Type) =
        let serialization, deserialization = typ |> Serialization.Create, typ |> Deserialization.Create
        let typeMap = TypeMap.Create(typ, deserialization, serialization, typ |> findBaseType isEncoded)
        if typeMaps.TryAdd(typ, typeMap) then
            let typeMap = typeMaps.[typ]
            try
                typeMap |> init
            // with
            //     TODO: generate exceptions for invalid typemap methods.
            finally
                typeMap.IsComplete <- true
            typeMap
        else typeMaps.[typ]
    match typ with
    | NotSerializable ->
        failwithf "Type `%s` is not serializable." typ.FullName
    | Serializable(typeAttribute) ->
        typ |> addTypeMap (fun typeMap ->
            match typeAttribute.Layout with
            | LayoutKind.Choice -> typeMap |> createChoiceTypeSerializers isEncoded (getContentOfChoice typeMap |> getProperties (getTypeMap isEncoded))
            | _ -> typeMap |> createTypeSerializers isEncoded
            )

and internal getTypeMap (isEncoded: bool) (typ: Type) : TypeMap =
    match typeMaps.TryGetValue(typ) with
    | true, typeMap -> typeMap
    | false, _ -> typ |> createTypeMap isEncoded

and findTypeMap isEncoded (typ: Type) =
    match typ.GetCustomAttribute<XRoadTypeAttribute>() with
    | null -> None
    | _ -> Some(typ |> getTypeMap isEncoded)

and findBaseType isEncoded (typ: Type) =
    if typ.BaseType |> isNull || typ.BaseType = typeof<obj> then None
    else match typ.BaseType |> findTypeMap isEncoded with
         | None -> typ.BaseType |> findBaseType isEncoded
         | typeMap -> typeMap

and findDirectSubTypes (isEncoded: bool) (typ: Type) : TypeMap list =
    typ.Assembly.GetTypes()
    |> List.ofArray
    |> List.filter (fun x -> x.BaseType = typ)
    |> List.choose (findTypeMap isEncoded)

let getCompleteTypeMap isEncoded typ =
    let typeMap = getTypeMap isEncoded typ
    while not typeMap.IsComplete do
        System.Threading.Thread.Sleep(100)
    typeMap

module internal XsdTypes =
    open NodaTime
    open NodaTime.Text
    open System.IO

    let readToNextWrapper (r: XmlReader) f =
        let depth = r.Depth
        let name = r.LocalName
        let ns = r.NamespaceURI
        let v = f()
        r.ReadToNextElement(name, ns, depth, false)
        v

    let serializeDefault (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(value)

    let serializeBigInteger (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(value.ToString())

    let serializeString (writer: XmlWriter, value: obj, _: SerializerContext) =
        if value |> isNull then writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        elif unbox value = "" then ()
        else writer.WriteValue(value)

    let serializePeriod (writer: XmlWriter, value: obj, _: SerializerContext) =
        if value |> isNull then writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true") else
        let period: Period = unbox value
        writer.WriteValue(PeriodPattern.NormalizingIso.Format(period))

    let serializeNullable (writer: XmlWriter) (value: obj) (context: SerializerContext) fser =
        if value |> isNull then writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        else fser(writer, value, context)

    let offsetDatePattern = OffsetDatePattern.CreateWithInvariantCulture("uuuu'-'MM'-'ddo<Z+HH:mm>")
    let offsetDateTimePattern = OffsetDateTimePattern.CreateWithInvariantCulture("uuuu'-'MM'-'dd'T'HH':'mm':'ss.FFFFFFFFFo<Z+HH:mm>")
    let offsetTimePattern = OffsetTimePattern.CreateWithInvariantCulture("HH':'mm':'ss.FFFFFFFFFo<Z+HH:mm>")

    let serializeOffsetDate (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(offsetDatePattern.Format(unbox value))

    let serializeOffsetDateTime (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(offsetDateTimePattern.Format(unbox value))

    let serializeOffsetTime (writer: XmlWriter, value: obj, _: SerializerContext) =
        writer.WriteValue(offsetTimePattern.Format(unbox value))

    let deserializeNullable (reader: XmlReader) (context: SerializerContext) fdeser =
        let nilValue = reader.GetAttribute("nil", XmlNamespace.Xsi)
        let nilValue = if nilValue |> isNull then "" else nilValue.ToLower()
        if nilValue = "1" || nilValue = "true" then null else fdeser(reader, context)

    let deserializeValue<'T> (reader: XmlReader) (_: SerializerContext) (fread: unit -> 'T) : obj =
        if reader.IsEmptyElement then box Unchecked.defaultof<'T>
        elif reader.Read() then fread() |> box
        else failwith "Unexpected end of SOAP message."

    let deserializeStringValue (reader: XmlReader, _: SerializerContext) : obj =
        if reader.IsEmptyElement then box ""
        elif reader.Read() then reader.ReadContentAsString() |> box
        else failwith "Unexpected end of SOAP message."

    let spaceRegex = System.Text.RegularExpressions.Regex("[ ]+")

    let deserializeTokenStringValue (reader: XmlReader, _: SerializerContext) : obj =
        if reader.IsEmptyElement then box ""
        elif reader.Read() then
            let v = reader.ReadContentAsString().Replace('\r', ' ').Replace('\n', ' ').Replace('\t', ' ')
            spaceRegex.Replace(v, " ").Trim(' ') |> box
        else failwith "Unexpected end of SOAP message."

    let deserializeDateValue (reader: XmlReader) (context: SerializerContext) : obj =
        let value = reader.ReadContentAsString()
        let od =
            match LocalDatePattern.Iso.Parse(value) with
            | result when result.Success -> OffsetDate(result.Value, context.DefaultOffset)
            | _ -> offsetDatePattern.Parse(value).GetValueOrThrow()
        box od

    let deserializeDateTimeValue (reader: XmlReader) (context: SerializerContext) : obj =
        let value = reader.ReadContentAsString()
        let odt =
            match LocalDateTimePattern.ExtendedIso.Parse(value) with
            | result when result.Success -> OffsetDateTime(result.Value, context.DefaultOffset)
            | _ -> offsetDateTimePattern.Parse(value).GetValueOrThrow()
        box odt

    let deserializeTimeValue (reader: XmlReader) (context: SerializerContext) : obj =
        let value = reader.ReadContentAsString()
        let odt =
            match LocalTimePattern.ExtendedIso.Parse(value) with
            | result when result.Success -> OffsetTime(result.Value, context.DefaultOffset)
            | _ -> offsetTimePattern.Parse(value).GetValueOrThrow()
        box odt

    let deserializePeriodValue (reader: XmlReader, _: SerializerContext) : obj =
        if reader.IsEmptyElement then box Period.Zero
        elif reader.Read() then PeriodPattern.NormalizingIso.Parse(reader.ReadContentAsString()).GetValueOrThrow() |> box
        else failwith "Unexpected end of SOAP message."

    let serializeNullableDefault (writer, value, context) = serializeNullable writer value context serializeDefault
    let serializeNullableBigInteger (writer, value, context) = serializeNullable writer value context serializeBigInteger
    let serializeNullableOffsetDate (writer, value, context) = serializeNullable writer value context serializeOffsetDate
    let serializeNullableOffsetDateTime (writer, value, context) = serializeNullable writer value context serializeOffsetDateTime
    let serializeNullableOffsetTime (writer, value, context) = serializeNullable writer value context serializeOffsetTime

    let deserializeBoolean (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context reader.ReadContentAsBoolean)
    let deserializeDecimal (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context reader.ReadContentAsDecimal)
    let deserializeDouble (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context reader.ReadContentAsDouble)
    let deserializeInt32 (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context reader.ReadContentAsInt)
    let deserializeInt64 (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context reader.ReadContentAsLong)
    let deserializeBigInteger (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context (reader.ReadContentAsDecimal >> BigInteger))
    let deserializeOffsetDate (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context (fun _ -> deserializeDateValue reader context))
    let deserializeOffsetDateTime (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context (fun _ -> deserializeDateTimeValue reader context))
    let deserializeOffsetTime (reader, context) = readToNextWrapper reader (fun () -> deserializeValue reader context (fun _ -> deserializeTimeValue reader context))

    let deserializeNullableBoolean (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeBoolean)
    let deserializeNullableDecimal (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeDecimal)
    let deserializeNullableDouble (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeDouble)
    let deserializeNullableInt32 (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeInt32)
    let deserializeNullableInt64 (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeInt64)
    let deserializeNullableBigInteger (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeBigInteger)
    let deserializeNullableOffsetDate (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeOffsetDate)
    let deserializeNullableOffsetDateTime (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeOffsetDateTime)
    let deserializeNullableOffsetTime (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeOffsetTime)
    let deserializePeriod (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializePeriodValue)
    let deserializeString (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeStringValue)
    let deserializeTokenString (reader, context) = readToNextWrapper reader (fun () -> deserializeNullable reader context deserializeTokenStringValue)

    let serializeBinaryContent (writer: XmlWriter, value: obj, context: SerializerContext) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ ->
            let content = unbox<BinaryContent> value
            if context.IsMultipart then
                context.AddAttachment(content.ContentID, content, false)
                writer.WriteAttributeString("href", sprintf "cid:%s" content.ContentID)
            else
                let bytes = (unbox<BinaryContent> value).GetBytes()
                writer.WriteBase64(bytes, 0, bytes.Length)

    let serializeXopBinaryContent(writer: XmlWriter, value: obj, context: SerializerContext) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ ->
            writer.WriteStartElement("xop", "Include", XmlNamespace.Xop)
            let content = unbox<BinaryContent> value
            context.AddAttachment(content.ContentID, content, true)
            writer.WriteAttributeString("href", sprintf "cid:%s" content.ContentID)
            writer.WriteEndElement()
            
    let serializeSwaRefBinaryContent(writer: XmlWriter, value: obj, context: SerializerContext) =
        match value with
        | null -> writer.WriteAttributeString("nil", XmlNamespace.Xsi, "true")
        | _ ->
            let content = unbox<BinaryContent> value
            context.AddAttachment(content.ContentID, content, true)
            writer.WriteString(sprintf "cid:%s" content.ContentID)

    let deserializeBinaryContent (reader: XmlReader, context: SerializerContext) = readToNextWrapper reader (fun () ->
        let nilValue = match reader.GetAttribute("nil", XmlNamespace.Xsi) with null -> "" | x -> x
        match nilValue.ToLower() with
        | "true" | "1" -> null
        | _ ->
            match reader.GetAttribute("href") with
            | null ->
                if reader.IsEmptyElement then BinaryContent.Create([| |]) else
                reader.Read() |> ignore
                let bufferSize = 4096
                let buffer = Array.zeroCreate<byte>(bufferSize)
                use stream = new MemoryStream()
                let rec readContents() =
                    let readCount = reader.ReadContentAsBase64(buffer, 0, bufferSize)
                    if readCount > 0 then stream.Write(buffer, 0, readCount)
                    if readCount = bufferSize then readContents()
                readContents()
                stream.Flush()
                stream.Position <- 0L
                BinaryContent.Create(stream.ToArray())
            | contentID -> context.GetAttachment(contentID)
    )

    let deserializeXopBinaryContent (reader: XmlReader, context: SerializerContext) = readToNextWrapper reader (fun () ->
        let nilValue = match reader.GetAttribute("nil", XmlNamespace.Xsi) with null -> "" | x -> x
        match nilValue.ToLower() with
        | "true" | "1" -> null
        | _ ->
            if reader.IsEmptyElement then BinaryContent.Create([| |]) else
            let depth = reader.Depth + 1
            let rec moveToXopInclude () =
                if reader.Read() then
                    if reader.NodeType = XmlNodeType.EndElement && reader.Depth < depth then false
                    elif reader.NodeType <> XmlNodeType.Element || reader.Depth <> depth || reader.LocalName <> "Include" || reader.NamespaceURI <> XmlNamespace.Xop then moveToXopInclude()
                    else true
                else false
            if moveToXopInclude () then
                match reader.GetAttribute("href") with
                | null -> failwithf "Missing reference to multipart content in xop:Include element."
                | contentID -> reader.Read() |> ignore; context.GetAttachment(contentID)
            else BinaryContent.Create([| |])
    )
    
    let deserializeSwaRefBinaryContent (reader: XmlReader, context: SerializerContext) = readToNextWrapper reader (fun () ->
        let nilValue = match reader.GetAttribute("nil", XmlNamespace.Xsi) with null -> "" | x -> x
        match nilValue.ToLower() with
        | "true" | "1" -> null
        | _ ->
            if reader.IsEmptyElement then BinaryContent.Create([| |]) else
            let contentId = reader.ReadElementContentAsString()
            context.GetAttachment(contentId)
    )

    let private addTypeMap typ ser deser =
        let typeMap = TypeMap.Create(getMarkedType typ, { Root = deser; Content = null; MatchType = null }, { Root = ser; Content = null }, None)
        typeMap.IsComplete <- true
        typeMaps.TryAdd(typ, typeMap) |> ignore

    let private addBinaryTypeMap typ ser deser =
        let typeMap = TypeMap.Create(typeof<BinaryContent>, { Root = deser; Content = null; MatchType = null }, { Root = ser; Content = null }, None)
        typeMap.IsComplete <- true
        typeMaps.TryAdd(typ, typeMap) |> ignore

    let mi e = match e with Call(_,mi,_) -> mi | _ -> failwith "do not use for that"

    let init () =
        addTypeMap typeof<bool> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeBoolean(null, null) @>)
        addTypeMap typeof<Nullable<bool>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableBoolean(null, null) @>)
        addTypeMap typeof<decimal> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeDecimal(null, null) @>)
        addTypeMap typeof<Nullable<decimal>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableDecimal(null, null) @>)
        addTypeMap typeof<double> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeDouble(null, null) @>)
        addTypeMap typeof<Nullable<double>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableDouble(null, null) @>)
        addTypeMap typeof<int32> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeInt32(null, null) @>)
        addTypeMap typeof<Nullable<int32>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableInt32(null, null) @>)
        addTypeMap typeof<int64> (mi <@ serializeDefault(null, null, null) @>) (mi <@ deserializeInt64(null, null) @>)
        addTypeMap typeof<Nullable<int64>> (mi <@ serializeNullableDefault(null, null, null) @>) (mi <@ deserializeNullableInt64(null, null) @>)
        addTypeMap typeof<BigInteger> (mi <@ serializeBigInteger(null, null, null) @>) (mi <@ deserializeBigInteger(null, null) @>)
        addTypeMap typeof<Nullable<BigInteger>> (mi <@ serializeNullableBigInteger(null, null, null) @>) (mi <@ deserializeNullableBigInteger(null, null) @>)
        addTypeMap typeof<OffsetDate> (mi <@ serializeOffsetDate(null, null, null) @>) (mi <@ deserializeOffsetDate(null, null) @>)
        addTypeMap typeof<Nullable<OffsetDate>> (mi <@ serializeNullableOffsetDate(null, null, null) @>) (mi <@ deserializeNullableOffsetDate(null, null) @>)
        addTypeMap typeof<OffsetDateTime> (mi <@ serializeOffsetDateTime(null, null, null) @>) (mi <@ deserializeOffsetDateTime(null, null) @>)
        addTypeMap typeof<Nullable<OffsetDateTime>> (mi <@ serializeNullableOffsetDateTime(null, null, null) @>) (mi <@ deserializeNullableOffsetDateTime(null, null) @>)
        addTypeMap typeof<OffsetTime> (mi <@ serializeOffsetTime(null, null, null) @>) (mi <@ deserializeOffsetTime(null, null) @>)
        addTypeMap typeof<Nullable<OffsetTime>> (mi <@ serializeNullableOffsetTime(null, null, null) @>) (mi <@ deserializeNullableOffsetTime(null, null) @>)
        addTypeMap typeof<Period> (mi <@ serializePeriod(null, null, null) @>) (mi <@ deserializePeriod(null, null) @>)
        addTypeMap typeof<string> (mi <@ serializeString(null, null, null) @>) (mi <@ deserializeString(null, null) @>)
        addTypeMap typeof<MarkerTypes.TokenString> (mi <@ serializeString(null, null, null) @>) (mi <@ deserializeTokenString(null, null) @>)
        addBinaryTypeMap typeof<BinaryContent> (mi <@ serializeBinaryContent(null, null, null) @>) (mi <@ deserializeBinaryContent(null, null) @>)
        addBinaryTypeMap typeof<MarkerTypes.XopAttachment> (mi <@ serializeXopBinaryContent(null, null, null) @>) (mi <@ deserializeXopBinaryContent(null, null) @>)
        addBinaryTypeMap typeof<MarkerTypes.SwaRefAttachment> (mi <@ serializeSwaRefBinaryContent(null, null, null) @>) (mi <@ deserializeSwaRefBinaryContent(null, null) @>)

module internal DynamicMethods =
    let requiredOpAttr<'T when 'T :> Attribute and 'T : null and 'T : equality> (mi: MethodInfo) : 'T =
        mi.GetCustomAttribute<'T>()
        |> Option.ofObj
        |> Option.defaultWith (fun _ -> failwithf "Operation should define `%s`." typeof<'T>.Name)

    let emitDeserializer (mi: MethodInfo) (responseAttr: XRoadResponseAttribute) : DeserializerDelegate =
        let returnType = responseAttr.ReturnType |> Option.ofObj |> Option.defaultValue mi.ReturnType
        let typeMap = getCompleteTypeMap responseAttr.Encoded returnType
        typeMap.DeserializeDelegate.Value

    let emitSerializer (mi: MethodInfo) (requestAttr: XRoadRequestAttribute) : OperationSerializerDelegate =
        let method = 
            DynamicMethod
                ( sprintf "serialize_%s" mi.Name,
                  null,
                  [| typeof<XmlWriter>; typeof<obj[]>; typeof<SerializerContext> |],
                  true )

        let parameters = getContentOfMethod mi |> getProperties (getCompleteTypeMap requestAttr.Encoded)
        method.GetILGenerator() |> (emit' {
            ldarg_0
            ldstr requestAttr.Name
            ldstr requestAttr.Namespace
            callvirt_expr <@ (null: XmlWriter).WriteStartElement("", "") @>
            merge (EmitSerialization.emitContentSerializerMethod requestAttr.Encoded parameters)
            ldarg_0
            callvirt_expr <@ (null: XmlWriter).WriteEndElement() @>
            ret
        }) |> ignore

        method.CreateDelegate(typeof<OperationSerializerDelegate>) |> unbox
        
    let createMethodMap (mi: MethodInfo) : MethodMap =
        let operationAttr = mi |> requiredOpAttr<XRoadOperationAttribute>
        let requestAttr = mi |> requiredOpAttr<XRoadRequestAttribute>
        let responseAttr = mi |> requiredOpAttr<XRoadResponseAttribute>
        let requiredHeadersAttr = mi.GetCustomAttribute<XRoadRequiredHeadersAttribute>() |> Option.ofObj

        { Deserializer = emitDeserializer mi responseAttr
          Serializer = emitSerializer mi requestAttr
          Request =
            { IsEncoded = requestAttr.Encoded
              IsMultipart = requestAttr.Multipart
              Accessor = Some(XmlQualifiedName(requestAttr.Name, requestAttr.Namespace)) }
          Response =
            { IsEncoded = responseAttr.Encoded
              IsMultipart = responseAttr.Multipart
              Accessor = Some(XmlQualifiedName(responseAttr.Name, responseAttr.Namespace)) }
          ServiceCode = operationAttr.ServiceCode
          ServiceVersion = operationAttr.ServiceVersion |> Option.ofObj
          Namespaces = []
          RequiredHeaders = dict [ match requiredHeadersAttr with
                                   | Some(attr) -> yield (attr.Namespace, attr.Names)
                                   | None -> () ] }

    let private methodMaps = ConcurrentDictionary<MethodInfo, MethodMap>()

    let internal getMethodMap mi =
        match methodMaps.TryGetValue(mi) with
        | true, mm -> mm
        | _ -> methodMaps.GetOrAdd(mi, (createMethodMap mi))

let getMethodMap = DynamicMethods.getMethodMap

do XsdTypes.init()
