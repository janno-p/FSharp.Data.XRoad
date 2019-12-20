namespace FSharp.Data.XRoad

open FSharp.Data.XRoad.Attributes
open NUnit.Framework
open System
open System.Reflection

[<AutoOpen>]
module internal Helpers =
    let assertTypeAttribute name ns isAnonymous layout (typ: Type) =
        let attr = typ.GetCustomAttribute<XRoadTypeAttribute>()
        Assert.IsNotNull(attr, sprintf "Provided type '%s' should have XRoadTypeAttribute defined." typ.Name)
        Assert.AreEqual(isAnonymous, attr.IsAnonymous)
        Assert.AreEqual(layout, attr.Layout)
        Assert.AreEqual(name, attr.Name)
        Assert.AreEqual(ns, attr.Namespace)
