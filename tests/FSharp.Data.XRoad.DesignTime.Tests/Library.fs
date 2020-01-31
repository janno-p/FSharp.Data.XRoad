module FSharp.Data.XRoad.DesignTime.Tests

open FSharp.Data.XRoad
open FSharp.Data.XRoad.MetaServices
open FSharp.Data.XRoad.Schema
open NUnit.Framework
open ProviderImplementation.ProvidedTypes
open System.Xml.Linq

let private generateTypes serviceId =
    let clientId = "SUBSYSTEM:ee-dev/GOV/70000310/kir-arendus" |> XRoadMemberIdentifier.Parse
    use stream = openWsdlStream Common.host clientId serviceId
    let document = XDocument.Load(stream)
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
