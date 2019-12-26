namespace FSharp.Data.XRoad

open FSharp.Data.XRoad.Attributes
open FSharp.Data.XRoad.Emit
open NUnit.Framework
open System
open System.IO
open System.Reflection
open System.Text
open System.Xml
open System.Xml.Linq

[<AutoOpen>]
module internal Helpers =
    let assertTypeAttribute name ns isAnonymous layout (typ: Type) =
        let attr = typ.GetCustomAttribute<XRoadTypeAttribute>()
        Assert.IsNotNull(attr, sprintf "Provided type '%s' should have XRoadTypeAttribute defined." typ.Name)
        Assert.AreEqual(isAnonymous, attr.IsAnonymous)
        Assert.AreEqual(layout, attr.Layout)
        Assert.AreEqual(name, attr.Name)
        Assert.AreEqual(ns, attr.Namespace)

    let serialize (serviceType: Type) producerNamespace context nm value =
        let map = serviceType.GetMethod(nm) |> getMethodMap
        use stream = new MemoryStream()
        use sw = new StreamWriter(stream, Encoding.UTF8)
        use writer = XmlWriter.Create(sw)
        writer.WriteStartDocument()
        writer.WriteStartElement("Body")
        writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
        writer.WriteAttributeString("xmlns", "tns", XmlNamespace.Xmlns, producerNamespace)
        writer.WriteAttributeString("xmlns", "test", XmlNamespace.Xmlns, "testns")
        map.Serializer.Invoke(writer, value, context)
        writer.WriteEndElement()
        writer.WriteEndDocument()
        writer.Flush()
        stream.Position <- 0L
        use reader = new StreamReader(stream, Encoding.UTF8)
        reader.ReadToEnd()

    let deserialize (serviceType: Type) context (nm: string) (xml: string) =
        let map = serviceType.GetMethod(nm) |> getMethodMap
        use textReader = new StringReader(xml)
        use reader = XmlReader.Create(textReader)
        while reader.Read() && not (reader.NodeType = XmlNodeType.Element && reader.Depth = 1) do ()
        map.Deserializer.Invoke(reader, context)
