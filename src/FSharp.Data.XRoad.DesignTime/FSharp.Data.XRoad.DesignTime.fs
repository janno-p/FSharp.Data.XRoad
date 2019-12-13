module FSharp.Data.XRoadImplementation

open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open FSharp.Data.XRoad
open ProviderImplementation.ProvidedTypes

// Put any utility helpers here
[<AutoOpen>]
module internal Helpers =
    let x = 1

    let (|ArrayOf3|) (args: obj array) =
        match args with
        | [| arg1; arg2; arg3 |] -> (unbox<'a> arg1, unbox<'b> arg2, unbox<'c> arg3)
        | _ -> failwith "never"

    let strToOption value =
        match value with null | "" -> None | _ -> Some(value)

[<TypeProvider>]
type XRoadServerProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("FSharp.Data.XRoad.DesignTime", "FSharp.Data.XRoad.Runtime")], addDefaultProbingLocation=true)

    let ns = "FSharp.Data.XRoad"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)

    let createNoteField message : MemberInfo =
        let field = ProvidedField.Literal("<Note>", typeof<string>, message)
        field.AddXmlDoc(message)
        upcast field

    let createServerInstanceType typeName (ArrayOf3 (securityServerUriString: string, clientIdentifierString: string, forceRefresh: bool)) =
        let instanceTy = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)

        let clientIdentifier = XRoadMemberIdentifier.Parse(clientIdentifierString)
        let xRoadInstance = clientIdentifier.XRoadInstance
        let memberClass = clientIdentifier.MemberClass
        let memberCode = clientIdentifier.MemberCode
        let subsystemCode = clientIdentifier.SubsystemCode

        let identifier = ProvidedProperty("Identifier", typeof<XRoadMemberIdentifier>, isStatic = true, getterCode = (fun _ -> <@@ XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode) @@>))
        instanceTy.AddMember(identifier)

        let identifierString = ProvidedField.Literal("IdentifierString", typeof<string>, clientIdentifierString)
        instanceTy.AddMember(identifierString)

        let uriField = ProvidedField.Literal("Uri", typeof<string>, securityServerUriString)
        instanceTy.AddMember(uriField)

        let instanceField = ProvidedField.Literal("XRoadInstance", typeof<string>, xRoadInstance)
        instanceTy.AddMember(instanceField)

        let memberClassField = ProvidedField.Literal("MemberClass", typeof<string>, memberClass)
        instanceTy.AddMember(memberClassField)

        let memberCodeField = ProvidedField.Literal("MemberCode", typeof<string>, memberCode)
        instanceTy.AddMember(memberCodeField)

        subsystemCode |> strToOption |> Option.iter (fun value ->
            let subsystemCodeField = ProvidedField.Literal("SubsystemCode", typeof<string>, value)
            instanceTy.AddMember(subsystemCodeField)
        )

        instanceTy

    let createTypes () =
        let serverTy = ProvidedTypeDefinition(asm, ns, "XRoadServer", Some typeof<obj>)
        serverTy.AddXmlDoc("Type provider which collects data from selected X-Road instance.")

        let staticParameters =
            [ ProvidedStaticParameter("SecurityServerUri", typeof<string>), "X-Road security server uri which is used to connect to that X-Road instance."
              ProvidedStaticParameter("ClientIdentifier", typeof<string>), "Client identifier used to access X-Road infrastructure (MEMBER or SUBSYSTEM)."
              ProvidedStaticParameter("ForceRefresh", typeof<bool>, false), "When `true`, forces type provider to refresh data from security server." ]
            |> List.map (fun (parameter,doc) -> parameter.AddXmlDoc(doc); parameter)

        serverTy.DefineStaticParameters(staticParameters, createServerInstanceType)

        [serverTy]

    do
        this.AddNamespace(ns, createTypes())

[<TypeProvider>]
type BasicErasingProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("FSharp.Data.XRoad.DesignTime", "FSharp.Data.XRoad.Runtime")], addDefaultProbingLocation=true)

    let ns = "MyNamespace"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)  

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)

        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string>)], invokeCode = fun args -> <@@ (%%(args.[0]):string) :> obj @@>)
        myType.AddMember(ctor2)

        let innerState = ProvidedProperty("InnerState", typeof<string>, getterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
        myType.AddMember(innerState)

        let meth = ProvidedMethod("StaticMethod", [], typeof<DataSource>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<DataSource>)))
        myType.AddMember(meth)

        let nameOf =
            let param = ProvidedParameter("p", typeof<Expr<int>>)
            param.AddCustomAttribute {
                new CustomAttributeData() with
                    member __.Constructor = typeof<ReflectedDefinitionAttribute>.GetConstructor([||])
                    member __.ConstructorArguments = [||] :> _
                    member __.NamedArguments = [||] :> _
            }
            ProvidedMethod("NameOf", [ param ], typeof<string>, isStatic = true, invokeCode = fun args ->
                <@@
                    match (%%args.[0]) : Expr<int> with
                    | Microsoft.FSharp.Quotations.Patterns.ValueWithName (_, _, n) -> n
                    | e -> failwithf "Invalid quotation argument (expected ValueWithName): %A" e
                @@>)
        myType.AddMember(nameOf)

        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<TypeProvider>]
type BasicGenerativeProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("FSharp.Data.XRoad.DesignTime", "FSharp.Data.XRoad.Runtime")])

    let ns = "FSharp.Data.XRoad"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)  

    let createType typeName (count:int) =
        let asm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)

        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string>)], invokeCode = fun args -> <@@ (%%(args.[1]):string) :> obj @@>)
        myType.AddMember(ctor2)

        for i in 1 .. count do 
            let prop = ProvidedProperty("Property" + string i, typeof<int>, getterCode = fun args -> <@@ i @@>)
            myType.AddMember(prop)

        let meth = ProvidedMethod("StaticMethod", [], typeof<DataSource>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<DataSource>)))
        myType.AddMember(meth)
        asm.AddTypes [ myType ]

        myType

    let myParamType = 
        let t = ProvidedTypeDefinition(asm, ns, "GenerativeProvider", Some typeof<obj>, isErased=false)
        t.DefineStaticParameters( [ProvidedStaticParameter("Count", typeof<int>)], fun typeName args -> createType typeName (unbox<int> args.[0]))
        t
    do
        this.AddNamespace(ns, [myParamType])


[<TypeProviderAssembly>]
do ()
