module FSharp.Data.XRoad.DesignTime.Tests.Library

open FSharp.Data.XRoad
open FSharp.Data.XRoad.DesignTime
open FSharp.Data.XRoad.MetaServices
open FSharp.Data.XRoad.Schema
open ProviderImplementation.ProvidedTypes
open System.Xml.Linq
open Xunit

let private generateTypesFiltered filter serviceId =
    let clientId = "SUBSYSTEM:ee-dev/GOV/70000310/kir-arendus" |> XRoadMemberIdentifier.Parse
    use stream = openWsdlStream Common.host clientId serviceId
    let document = XDocument.Load(stream)
    let schema = ProducerDescription.Load(document, "en", filter)
    let asm = ProvidedAssembly()
    let serviceTy = ProvidedTypeDefinition(asm, "FSharp.Data.XRoad", "GenerateTypesUsingMetaService", Some typeof<obj>, isErased=false)
    serviceTy.AddMembers(Builder.buildServiceTypeMembers schema)
    asm.AddTypes([serviceTy])

let private generateTypes =
    generateTypesFiltered []

[<Fact>]
let rr () =
    let producerId = "SUBSYSTEM:ee-dev/GOV/70008440/rr" |> XRoadMemberIdentifier.Parse
    let serviceId = XRoadServiceIdentifier(producerId, "RRAddress", "v1")
    generateTypes serviceId

[<Fact>]
let estat () =
    let producerId = "SUBSYSTEM:ee-dev/GOV/70000332/estat" |> XRoadMemberIdentifier.Parse
    let serviceId = XRoadServiceIdentifier(producerId, "SubmitData", "v1")
    generateTypes serviceId

[<Fact>]
let ehis () =
    let producerId = "SUBSYSTEM:ee-dev/GOV/70000740/ehis" |> XRoadMemberIdentifier.Parse
    let serviceId = XRoadServiceIdentifier(producerId, "eeIsikukaart", "v1")
    generateTypesFiltered ["eeIsikukaart"] serviceId

[<Fact>]
let kutseregister () =
    let producerId = "SUBSYSTEM:ee-dev/COM/10126529/kutseregister" |> XRoadMemberIdentifier.Parse
    let serviceId = XRoadServiceIdentifier(producerId, "kutsetunnistus", "v2")
    generateTypes serviceId

[<Fact>]
let kma () =
    let producerId = "SUBSYSTEM:ee-dev/GOV/70008747/itdak" |> XRoadMemberIdentifier.Parse
    let serviceId = XRoadServiceIdentifier(producerId, "isikut_toendavate13", "v1")
    generateTypesFiltered ["isikut_toendavate13"] serviceId

[<Fact>]
let ``kp saatmine`` () =
    let producerId = "SUBSYSTEM:ee-dev/GOV/74000091/kirst" |> XRoadMemberIdentifier.Parse
    let serviceId = XRoadServiceIdentifier(producerId, "kp_saatmine", "v1")
    generateTypesFiltered ["kp_saatmine"] serviceId
