module FSharp.Data.XRoadImplementation

open System
open System.Collections.Concurrent
open System.Net
open System.Reflection
open System.Security.Cryptography
open System.Text
open System.Xml.Linq
open FSharp.Core.CompilerServices
open FSharp.Data.XRoad
open FSharp.Data.XRoad.MetaServices
open FSharp.Data.XRoad.Schema
open ProviderImplementation.ProvidedTypes

[<AutoOpen>]
module internal Helpers =
    let (|ArrayOf3|) (args: obj array) =
        match args with
        | [| arg1; arg2; arg3 |] -> (unbox<'a> arg1, unbox<'b> arg2, unbox<'c> arg3)
        | _ -> failwith "never"

    let (|ArrayOf5|) (args: obj array) =
        match args with
        | [| arg1; arg2; arg3; arg4; arg5 |] -> (unbox<'a> arg1, unbox<'b> arg2, unbox<'c> arg3, unbox<'d> arg4, unbox<'e> arg5)
        | _ -> failwith "never"

    let parseOperationFilters : string -> string list = function
        | null -> []
        | value -> value.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> x.Trim()) |> Array.toList

    let toStaticParams def =
        def |> List.map (fun (parameter: ProvidedStaticParameter, doc) -> parameter.AddXmlDoc(doc); parameter)

[<TypeProvider>]
type XRoadInstanceProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("FSharp.Data.XRoad.DesignTime", "FSharp.Data.XRoad")], addDefaultProbingLocation=true)

    let ns = "FSharp.Data.XRoad"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<XRoadMemberIdentifier>.Assembly.GetName().Name = asm.GetName().Name)

    let createNoteField message : MemberInfo =
        let field = ProvidedField.Literal("<Note>", typeof<string>, message)
        field.AddXmlDoc(message)
        upcast field

    let createServiceTy securityServerUri (clientId: XRoadMemberIdentifier) (serviceId: XRoadServiceIdentifier) =
        let versionSuffix = serviceId.ServiceVersion |> strToOption |> Option.map (sprintf "/%s") |> Option.defaultValue ""
        let serviceName = sprintf "%s%s" serviceId.ServiceCode versionSuffix
        let serviceTy = ProvidedTypeDefinition(serviceName, Some typeof<obj>, hideObjectMethods=true)
        serviceTy.AddMembersDelayed (fun _ -> [
            let (c1, c2, c3, c4) = (clientId.XRoadInstance, clientId.MemberClass, clientId.MemberCode, clientId.SubsystemCode)
            let (s1, s2, s3, s4, s5, s6) = (serviceId.Owner.XRoadInstance, serviceId.Owner.MemberClass, serviceId.Owner.MemberCode, serviceId.Owner.SubsystemCode, serviceId.ServiceCode, serviceId.ServiceVersion)

            yield ProvidedProperty("Identifier", typeof<XRoadServiceIdentifier>, isStatic=true, getterCode=(fun _ -> <@@ XRoadServiceIdentifier(XRoadMemberIdentifier(s1, s2, s3, s4), s5, s6) @@>)) :> MemberInfo

            yield ProvidedMethod("GetWsdl", [], typeof<string>, isStatic=true, invokeCode=(fun _ -> <@@ downloadWsdl securityServerUri (XRoadMemberIdentifier(c1, c2, c3, c4)) (XRoadServiceIdentifier(XRoadMemberIdentifier(s1, s2, s3, s4), s5, s6)) @@>)) :> MemberInfo

            yield ProvidedField.Literal("IdentifierString", typeof<string>, serviceId.ToString()) :> MemberInfo
            yield ProvidedField.Literal("ServiceCode", typeof<string>, serviceId.ServiceCode) :> MemberInfo

            match serviceId.ServiceVersion |> strToOption with
            | Some(versionValue) ->
                yield ProvidedField.Literal("ServiceVersion", typeof<string>, versionValue) :> MemberInfo
            | None -> ()
        ])
        serviceTy

    let getServicesFromOwner securityServerUri clientId (ownerId: XRoadMemberIdentifier) =
        try
            Http.downloadMethodsList (Uri(securityServerUri)) clientId (XRoadServiceIdentifier(ownerId, "listMethods"))
            |> List.map (fun serviceId -> createServiceTy securityServerUri clientId serviceId :> MemberInfo)
        with e -> [e.ToString() |> createNoteField]

    let createXRoadSubsystemType securityServerUri clientId (subsystemId: XRoadMemberIdentifier) =
        let (xRoadInstance, memberClass, memberCode, subsystemCode) = (subsystemId.XRoadInstance, subsystemId.MemberClass, subsystemId.MemberCode, subsystemId.SubsystemCode)
        let subsystemTy = ProvidedTypeDefinition(subsystemCode, Some typeof<obj>, hideObjectMethods=true)
        subsystemTy.AddXmlDoc (sprintf "Subsystem %s." subsystemCode)
        subsystemTy.AddMembersDelayed(fun _ -> [
            yield ProvidedField.Literal("Name", typeof<string>, subsystemCode) :> MemberInfo
            yield ProvidedProperty("Identifier", typeof<XRoadMemberIdentifier>, isStatic=true, getterCode=(fun _ -> <@@ XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode) @@>)) :> MemberInfo
            yield ProvidedField.Literal("IdentifierString", typeof<string>, subsystemId.ToString()) :> MemberInfo

            match getServicesFromOwner securityServerUri clientId subsystemId with
            | [] -> ()
            | services ->
                let servicesTy = ProvidedTypeDefinition("Services", Some typeof<obj>, hideObjectMethods=true)
                servicesTy.AddXmlDoc(sprintf "Services defined for subsystem %s." subsystemCode)
                servicesTy.AddMembers(services)
                yield servicesTy :> MemberInfo
        ])
        subsystemTy

    let createXRoadMemberType securityServerUri clientId xRoadInstance xRoadMemberClassName (xRoadMember: Http.XRoadMember) =
        let xRoadMemberCode = xRoadMember.Code
        let xRoadMemberId = XRoadMemberIdentifier(xRoadInstance, xRoadMemberClassName, xRoadMember.Code)
        let xRoadMemberTy = ProvidedTypeDefinition(sprintf "%s (%s)" xRoadMember.Name xRoadMember.Code, Some typeof<obj>, hideObjectMethods=true)
        xRoadMemberTy.AddXmlDoc(xRoadMember.Name)
        xRoadMemberTy.AddMembersDelayed(fun _ -> [
            yield ProvidedField.Literal("Name", typeof<string>, xRoadMember.Name) :> MemberInfo
            yield ProvidedField.Literal("Code", typeof<string>, xRoadMember.Code) :> MemberInfo
            yield ProvidedProperty("Identifier", typeof<XRoadMemberIdentifier>, isStatic=true, getterCode=(fun _ -> <@@ XRoadMemberIdentifier(xRoadInstance, xRoadMemberClassName, xRoadMemberCode) @@>)) :> MemberInfo
            yield ProvidedField.Literal("IdentifierString", typeof<string>, XRoadMemberIdentifier(xRoadInstance, xRoadMemberClassName, xRoadMemberCode).ToString()) :> MemberInfo

            match getServicesFromOwner securityServerUri clientId xRoadMemberId with
            | [] -> ()
            | services ->
                let servicesTy = ProvidedTypeDefinition("Services", Some typeof<obj>, hideObjectMethods=true)
                servicesTy.AddXmlDoc(sprintf "Services defined for X-Road member %s (%s)." xRoadMember.Name xRoadMember.Code)
                servicesTy.AddMembers(services)
                yield servicesTy :> MemberInfo

            match xRoadMember.Subsystems with
            | [] -> ()
            | subsystems ->
                let subsystemsTy = ProvidedTypeDefinition("Subsystems", Some typeof<obj>, hideObjectMethods=true)
                subsystemsTy.AddXmlDoc(sprintf "Subsystems defined for X-Road member %s (%s)." xRoadMember.Name xRoadMember.Code)
                subsystemsTy.AddMembersDelayed (fun _ ->
                    subsystems
                    |> List.map (fun subsystem ->
                        let subsystemId = XRoadMemberIdentifier(xRoadMemberId.XRoadInstance, xRoadMemberId.MemberClass, xRoadMemberId.MemberCode, subsystem)
                        createXRoadSubsystemType securityServerUri clientId subsystemId :> MemberInfo
                    )
                )
                yield subsystemsTy :> MemberInfo
        ])
        xRoadMemberTy

    let createXRoadMemberClassType securityServerUri clientId xRoadInstance (xRoadMemberClass: Http.XRoadMemberClass) =
        let xRoadMemberClassTy = ProvidedTypeDefinition(xRoadMemberClass.Name, Some typeof<obj>, hideObjectMethods=true)
        xRoadMemberClassTy.AddXmlDoc(xRoadMemberClass.Name)
        xRoadMemberClassTy.AddMember(ProvidedField.Literal("Name", typeof<string>, xRoadMemberClass.Name))
        xRoadMemberClassTy.AddMembersDelayed (fun _ ->
            xRoadMemberClass.Members |> List.map (fun xRoadMember -> createXRoadMemberType securityServerUri clientId xRoadInstance xRoadMemberClass.Name xRoadMember :> MemberInfo)
        )
        xRoadMemberClassTy

    let createProducersType securityServerUri clientId xRoadInstance forceRefresh =
        let producersTy = ProvidedTypeDefinition("Producers", Some typeof<obj>, hideObjectMethods=true)
        producersTy.AddXmlDoc("All available producers in particular v6 X-Road instance.")
        producersTy.AddMembersDelayed (fun _ ->
            try
                Http.downloadProducerList (Uri(securityServerUri)) xRoadInstance forceRefresh
                |> List.map (fun xRoadMemberClass -> createXRoadMemberClassType securityServerUri clientId xRoadInstance xRoadMemberClass :> MemberInfo)
            with e -> [e.ToString() |> createNoteField]
        )
        producersTy

    let createCentralServicesType securityServerUri xRoadInstance forceRefresh =
        let centralServicesTy = ProvidedTypeDefinition("CentralServices", Some typeof<obj>, hideObjectMethods=true)
        centralServicesTy.AddXmlDoc("All available central services in particular v6 X-Road instance.")
        centralServicesTy.AddMembersDelayed (fun _ ->
            try
                match Http.downloadCentralServiceList securityServerUri xRoadInstance forceRefresh with
                | [] -> [createNoteField "No central services are listed in this X-Road instance."]
                | services ->
                    services |> List.map (fun serviceCode ->
                        let centralServiceType = ProvidedTypeDefinition(serviceCode, Some typeof<obj>, hideObjectMethods=true)
                        centralServiceType.AddMembersDelayed(fun _ -> [
                            yield ProvidedField.Literal("Name", typeof<string>, serviceCode) :> MemberInfo
                            yield ProvidedProperty("Identifier", typeof<XRoadCentralServiceIdentifier>, isStatic=true, getterCode=(fun _ -> <@@ XRoadCentralServiceIdentifier(xRoadInstance, serviceCode) @@>)) :> MemberInfo
                            yield ProvidedField.Literal("IdentifierString", typeof<obj>, XRoadCentralServiceIdentifier(xRoadInstance, serviceCode).ToString()) :> MemberInfo
                        ])
                        upcast centralServiceType
                    )
            with e -> [e.ToString() |> createNoteField]
        )
        centralServicesTy

    let createServerInstanceType typeName (ArrayOf3 (securityServerUriString: string, clientIdentifierString: string, forceRefresh: bool)) =
        let instanceTy = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)

        let clientId = XRoadMemberIdentifier.Parse(clientIdentifierString)
        let xRoadInstance = clientId.XRoadInstance
        let memberClass = clientId.MemberClass
        let memberCode = clientId.MemberCode
        let subsystemCode = clientId.SubsystemCode

        let identifier = ProvidedProperty("Identifier", typeof<XRoadMemberIdentifier>, isStatic=true, getterCode=(fun _ -> <@@ XRoadMemberIdentifier(xRoadInstance, memberClass, memberCode, subsystemCode) @@>))
        instanceTy.AddMember(identifier)

        let identifier = ProvidedProperty("Uri", typeof<Uri>, isStatic=true, getterCode=(fun _ -> <@@ Uri(securityServerUriString) @@>))
        instanceTy.AddMember(identifier)

        let identifierString = ProvidedField.Literal("IdentifierString", typeof<string>, clientIdentifierString)
        instanceTy.AddMember(identifierString)

        let uriString = ProvidedField.Literal("UriString", typeof<string>, securityServerUriString)
        instanceTy.AddMember(uriString)

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

        let securityServerUri = Uri(securityServerUriString)

        // Type which holds information about producers defined in selected instance.
        instanceTy.AddMember(createProducersType securityServerUriString clientId xRoadInstance forceRefresh)

        // Type which holds information about central services defined in selected instance.
        instanceTy.AddMember(createCentralServicesType securityServerUri xRoadInstance forceRefresh)

        instanceTy

    let createTypes () =
        let serverTy = ProvidedTypeDefinition(asm, ns, "LoadXRoadInstance", Some typeof<obj>)
        serverTy.AddXmlDoc("Type provider which collects data from selected X-Road instance.")

        let staticParameters =
            [ ProvidedStaticParameter("SecurityServerUri", typeof<string>), "X-Road security server uri which is used to connect to that X-Road instance."
              ProvidedStaticParameter("ClientIdentifier", typeof<string>), "Client identifier used to access X-Road infrastructure (MEMBER or SUBSYSTEM)."
              ProvidedStaticParameter("ForceRefresh", typeof<bool>, false), "When `true`, forces type provider to refresh data from security server." ]
            |> List.map (fun (parameter,doc) -> parameter.AddXmlDoc(doc); parameter)

        serverTy.DefineStaticParameters(staticParameters, createServerInstanceType)

        [serverTy]

    do this.AddNamespace(ns, createTypes())

[<TypeProvider>]
type XRoadServiceProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("FSharp.Data.XRoad.DesignTime", "FSharp.Data.XRoad")])

    let ns = "FSharp.Data.XRoad"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<XRoadMemberIdentifier>.Assembly.GetName().Name = asm.GetName().Name)

    let typeCache = ConcurrentDictionary<string, ProvidedTypeDefinition>()

    let generateServiceType typeName (schema: ProducerDescription) =
        let asm = ProvidedAssembly()
        let serviceTy = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)
        serviceTy.AddMembers(Builder.buildServiceTypeMembers schema)
        asm.AddTypes([serviceTy])
        serviceTy

    let reloadOrGenerateServiceType key typeName getSchema =
        match typeCache.TryGetValue(key) with
        | false, _ ->
            typeCache.GetOrAdd(key, (getSchema >> generateServiceType typeName))
        | true, typ -> typ

    let generateInstanceUsingMetaService typeName (ArrayOf5 (securityServerUri: string, clientId: string, serviceId: string, languageCode: string, filter: string)) =
        let key = sprintf "%s:%s:%s:%s:%s:%s" typeName securityServerUri clientId serviceId languageCode filter
        reloadOrGenerateServiceType key typeName (fun _ ->
            let filter = filter |> parseOperationFilters
            let clientId = XRoadMemberIdentifier.Parse(clientId)
            let serviceId = XRoadServiceIdentifier.Parse(serviceId)
            use stream = openWsdlStream securityServerUri clientId serviceId
            let document = XDocument.Load(stream)
            ProducerDescription.Load(document, languageCode, filter)
        )

    let generateInstanceUsingServiceDescription typeName (ArrayOf3 (uri: string, languageCode: string, filter: string)) =
        let key = sprintf "%s:%s:%s:%s" typeName uri languageCode filter
        reloadOrGenerateServiceType key typeName (fun _ ->
            let filter = filter |> parseOperationFilters
            ProducerDescription.Load(Http.resolveUri uri, languageCode, filter)
        )

    let computeHash =
        let sha1 = SHA1.Create()
        (fun (input: string) ->
            let hash = sha1.ComputeHash(Encoding.UTF8.GetBytes(input))
            Convert.ToBase64String(hash)
        )

    let generateInstanceFromString typeName (ArrayOf3 (input: string, languageCode: string, filter: string)) =
        let key = sprintf "%s:%s:%s:%s" typeName (computeHash input) languageCode filter
        reloadOrGenerateServiceType key typeName (fun _ ->
            let filter = filter |> parseOperationFilters
            ProducerDescription.Load(XDocument.Parse(input), languageCode, filter)
        )

    let metaServiceStaticParameters = [
        ProvidedStaticParameter("SecurityServerUri", typeof<string>), "Security server uri which is used to access X-Road meta services."
        ProvidedStaticParameter("ClientIdentifier", typeof<string>), "Client identifier used to access X-Road infrastructure (MEMBER or SUBSYSTEM)."
        ProvidedStaticParameter("ServiceIdentifier", typeof<string>), "Service identifier (in format of `SERVICE:EE/BUSINESS/123456789/highsecurity/getSecureData/v1`)."
        ProvidedStaticParameter("LanguageCode", typeof<string>, "et"), "Specify language code that is extracted as documentation tooltips. Default value is estonian (et)."
        ProvidedStaticParameter("Filter", typeof<string>, ""), "Comma separated list of operations which should be included in definitions. By default, all operations are included."
    ]

    let serviceDescriptionStaticParameters = [
        ProvidedStaticParameter("Uri", typeof<string>), "WSDL document location (either local file or network resource)."
        ProvidedStaticParameter("LanguageCode", typeof<string>, "et"), "Specify language code that is extracted as documentation tooltips. Default value is estonian (et)."
        ProvidedStaticParameter("Filter", typeof<string>, ""), "Comma separated list of operations which should be included in definitions. By default, all operations are included."
    ]

    let fromStringStaticParameters = [
        ProvidedStaticParameter("Input", typeof<string>), "WSDL document as string input."
        ProvidedStaticParameter("LanguageCode", typeof<string>, "et"), "Specify language code that is extracted as documentation tooltips. Default value is estonian (et)."
        ProvidedStaticParameter("Filter", typeof<string>, ""), "Comma separated list of operations which should be included in definitions. By default, all operations are included."
    ]

    let createTypes () = [
        let metaServiceGeneratorType = ProvidedTypeDefinition(asm, ns, "GenerateTypesUsingMetaService", Some typeof<obj>, isErased=false)
        metaServiceGeneratorType.AddXmlDoc("Type provider for generating service interfaces and data types for specific X-Road producer using security server meta services (getWsdl).")
        metaServiceGeneratorType.DefineStaticParameters(metaServiceStaticParameters |> toStaticParams, generateInstanceUsingMetaService)
        yield metaServiceGeneratorType

        let serviceDescriptionGeneratorType = ProvidedTypeDefinition(asm, ns, "GenerateTypesUsingServiceDescription", Some typeof<obj>, isErased=false)
        serviceDescriptionGeneratorType.AddXmlDoc("Type provider for generating service interfaces and data types for specific X-Road producer.")
        serviceDescriptionGeneratorType.DefineStaticParameters(serviceDescriptionStaticParameters |> toStaticParams, generateInstanceUsingServiceDescription)
        yield serviceDescriptionGeneratorType

        let fromStringGeneratorType = ProvidedTypeDefinition(asm, ns, "GenerateTypesFromString", Some typeof<obj>, isErased=false)
        fromStringGeneratorType.AddXmlDoc("Type provider for generating service interfaces and data types for specific string input.")
        fromStringGeneratorType.DefineStaticParameters(fromStringStaticParameters |> toStaticParams, generateInstanceFromString)
        yield fromStringGeneratorType
    ]

    do this.AddNamespace(ns, createTypes())

[<TypeProviderAssembly>]
do ()
