module FSharp.Data.XRoad.DesignTime.Tests

open FSharp.Data.XRoad
open FSharp.Data.XRoad.MetaServices
open FSharp.Data.XRoad.Schema
open NUnit.Framework
open ProviderImplementation.ProvidedTypes
open System.Xml.Linq

[<Test>]
let ``qqq`` () =
    let clientId = "SUBSYSTEM:ee-dev/GOV/70000310/kir-arendus" |> XRoadMemberIdentifier.Parse
    let producerId = "SUBSYSTEM:ee-dev/GOV/70008440/rr" |> XRoadMemberIdentifier.Parse
    let serviceId = XRoadServiceIdentifier(producerId, "RRAddress", "v1")
    use stream = openWsdlStream Common.host clientId serviceId
    let document = XDocument.Load(stream)
    let schema = ProducerDescription.Load(document, "en", [])
    let asm = ProvidedAssembly()
    let serviceTy = ProvidedTypeDefinition(asm, "FSharp.Data.XRoad", "GenerateTypesUsingMetaService", Some typeof<obj>, isErased=false)
    serviceTy.AddMembers(Builder.buildServiceTypeMembers schema)
    asm.AddTypes([serviceTy])
