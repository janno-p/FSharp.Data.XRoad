module FSharp.Data.XRoad.XRoadServerTests

open FSharp.Data.XRoad
open NUnit.Framework

type MemberServerType = XRoadServer<"urn:securityserver", "MEMBER:EE/BUSINESS/123456789">
type SubsystemServerType = XRoadServer<"urn:securityserver", "SUBSYSTEM:EE/BUSINESS/123456789/generic-consumer">

[<Test>]
let ``Static parameters are converted into member properties`` () =
    Assert.AreEqual("urn:securityserver", MemberServerType.Uri)
    Assert.AreEqual("MEMBER:EE/BUSINESS/123456789", MemberServerType.IdentifierString)
    Assert.AreEqual("EE", MemberServerType.XRoadInstance)
    Assert.AreEqual("BUSINESS", MemberServerType.MemberClass)
    Assert.AreEqual("123456789", MemberServerType.MemberCode)
    // Assert.AreEqual("generic-consumer", MemberServerType.SubsystemCode) // should not exist
    Assert.AreEqual("MEMBER:EE/BUSINESS/123456789", MemberServerType.Identifier.ToString())


[<Test>]
let ``Static parameters are converted into subsystem properties`` () =
    Assert.AreEqual("urn:securityserver", SubsystemServerType.Uri)
    Assert.AreEqual("SUBSYSTEM:EE/BUSINESS/123456789/generic-consumer", SubsystemServerType.IdentifierString)
    Assert.AreEqual("EE", SubsystemServerType.XRoadInstance)
    Assert.AreEqual("BUSINESS", SubsystemServerType.MemberClass)
    Assert.AreEqual("123456789", SubsystemServerType.MemberCode)
    Assert.AreEqual("generic-consumer", SubsystemServerType.SubsystemCode)
    Assert.AreEqual("SUBSYSTEM:EE/BUSINESS/123456789/generic-consumer", SubsystemServerType.Identifier.ToString())
