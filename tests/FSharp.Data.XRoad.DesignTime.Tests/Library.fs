﻿module FSharp.Data.XRoad.DesignTime.Tests

open System.IO
open FSharp.Data.XRoad
open FSharp.Data.XRoad.MetaServices
open FSharp.Data.XRoad.Schema
open NUnit.Framework
open ProviderImplementation.ProvidedTypes
open System.Xml.Linq

let private downloadWsdlToPath path clientId serviceId =
    let t = downloadWsdl Common.host clientId serviceId
    File.WriteAllText(path, t)

let private openOrDownloadWsdlStream clientId (serviceId: XRoadServiceIdentifier) : Stream =
    let fileName = System.String.Join("_", [
        serviceId.Owner.MemberClass
        serviceId.Owner.MemberCode
        serviceId.Owner.SubsystemCode
        serviceId.ServiceCode
        serviceId.ServiceVersion
    ])
    let localPath = Path.Combine(__SOURCE_DIRECTORY__, "wsdl", (fileName + ".wsdl"))
    if not (File.Exists localPath) then
        downloadWsdlToPath localPath clientId serviceId
    File.OpenRead(localPath)

let private generateTypesFiltered filter serviceId =
    let clientId = "SUBSYSTEM:ee-dev/GOV/70000310/kir-arendus" |> XRoadMemberIdentifier.Parse
    use stream = openOrDownloadWsdlStream clientId serviceId
    let document = XDocument.Load(stream)
    let schema = ProducerDescription.Load(document, "en", filter)
    let asm = ProvidedAssembly()
    let serviceTy = ProvidedTypeDefinition(asm, "FSharp.Data.XRoad", "GenerateTypesUsingMetaService", Some typeof<obj>, isErased=false)
    serviceTy.AddMembers(Builder.buildServiceTypeMembers schema)
    asm.AddTypes([serviceTy])

let private generateTypes =
    generateTypesFiltered []

let private generateTypesFromFile path =
    let document = XDocument.Parse(File.ReadAllText(path))
    let schema = ProducerDescription.Load(document, "en", [])
    let asm = ProvidedAssembly()
    let serviceTy = ProvidedTypeDefinition(asm, "FSharp.Data.XRoad", "GenerateTypesUsingMetaService", Some typeof<obj>, isErased=false)
    serviceTy.AddMembers(Builder.buildServiceTypeMembers schema)
    asm.AddTypes([serviceTy])

[<Test>]
let ``rr`` () =
    let producerId = "SUBSYSTEM:ee-dev/GOV/70008440/rr" |> XRoadMemberIdentifier.Parse
    let serviceId = XRoadServiceIdentifier(producerId, "RRAddress", "v1")
    generateTypes serviceId

[<Test>]
let ``estat`` () =
    let producerId = "SUBSYSTEM:ee-dev/GOV/70000332/estat" |> XRoadMemberIdentifier.Parse
    let serviceId = XRoadServiceIdentifier(producerId, "SubmitData", "v1")
    generateTypes serviceId

[<Test>]
let ``ehis`` () =
    let producerId = "SUBSYSTEM:ee-dev/GOV/70000740/ehis" |> XRoadMemberIdentifier.Parse
    let serviceId = XRoadServiceIdentifier(producerId, "eeIsikukaart", "v1")
    generateTypesFiltered ["eeIsikukaart"] serviceId

[<Test>]
let ``kutseregister`` () =
    let producerId = "SUBSYSTEM:ee-dev/COM/10126529/kutseregister" |> XRoadMemberIdentifier.Parse
    let serviceId = XRoadServiceIdentifier(producerId, "kutsetunnistus", "v2")
    generateTypes serviceId

[<Test>]
let ``kma`` () =
    let producerId = "SUBSYSTEM:ee-dev/GOV/70008747/itdak" |> XRoadMemberIdentifier.Parse
    let serviceId = XRoadServiceIdentifier(producerId, "isikut_toendavate13", "v1")
    generateTypesFiltered ["isikut_toendavate13"] serviceId
