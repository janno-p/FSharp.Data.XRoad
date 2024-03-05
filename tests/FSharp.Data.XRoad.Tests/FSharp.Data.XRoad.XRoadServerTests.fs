module FSharp.Data.XRoad.XRoadServerTests

open FSharp.Data.XRoad
open FsUnit.Xunit
open FsUnitTyped
open Xunit

type MemberServerType = LoadXRoadInstance<"urn:securityserver", "MEMBER:EE/BUSINESS/123456789">
type SubsystemServerType = LoadXRoadInstance<"urn:securityserver", "SUBSYSTEM:EE/BUSINESS/123456789/generic-consumer">

[<Fact>]
let ``Static parameters are converted into member properties`` () =
    MemberServerType.UriString |> shouldEqual "urn:securityserver"
    MemberServerType.IdentifierString |> shouldEqual "MEMBER:EE/BUSINESS/123456789"
    MemberServerType.XRoadInstance |> shouldEqual "EE"
    MemberServerType.MemberClass |> shouldEqual "BUSINESS"
    MemberServerType.MemberCode |> shouldEqual "123456789"
    // MemberServerType.SubsystemCode |> shouldEqual "generic-consumer" // should not exist
    MemberServerType.Identifier.ToString() |> shouldEqual "MEMBER:EE/BUSINESS/123456789"

[<Fact>]
let ``Static parameters are converted into subsystem properties`` () =
    SubsystemServerType.UriString |> shouldEqual "urn:securityserver"
    SubsystemServerType.IdentifierString |> shouldEqual "SUBSYSTEM:EE/BUSINESS/123456789/generic-consumer"
    SubsystemServerType.XRoadInstance |> shouldEqual "EE"
    SubsystemServerType.MemberClass |> shouldEqual "BUSINESS"
    SubsystemServerType.MemberCode |> shouldEqual "123456789"
    SubsystemServerType.SubsystemCode |> shouldEqual "generic-consumer"
    SubsystemServerType.Identifier.ToString() |> shouldEqual "SUBSYSTEM:EE/BUSINESS/123456789/generic-consumer"
