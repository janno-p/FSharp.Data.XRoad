namespace FSharp.Data.XRoad.Protocol

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Emit
open FSharp.Data.XRoad.Extensions
open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Security
open System.Reflection
open System.Xml
open System.Xml.XPath

[<AutoOpen>]
module internal Helpers =
    let [<Literal>] FAULT_PATH = "/*[local-name()='Envelope' and namespace-uri()='http://schemas.xmlsoap.org/soap/envelope/']/*[local-name()='Body' and namespace-uri()='http://schemas.xmlsoap.org/soap/envelope/']/*[faultCode|faultString]"

type internal XRoadFault(faultCode: string, faultString) =
    inherit Exception(faultString)
    member val FaultCode = faultCode with get
    member val FaultString = faultString with get

type internal XRoadResponse(endpoint: AbstractEndpointDeclaration, request: XRoadRequest, methodMap: MethodMap) =
    let response: WebResponse = request.GetResponse()
    let stream = new MemoryStream()

    let checkXRoadFault (stream: Stream) =
        stream.Position <- 0L
        use reader = XmlReader.Create(stream)
        let doc = XPathDocument(reader)
        let nav = doc.CreateNavigator()
        match nav.SelectSingleNode(FAULT_PATH) with
        | null -> ()
        | node ->
            let faultCode = node.SelectSingleNode("./faultCode")
            let faultString = node.SelectSingleNode("./faultString")
            let nodeToString = Option.ofObj >> Option.map (fun x -> (x: XPathNavigator).InnerXml) >> Option.defaultValue ""
            raise (XRoadFault(faultCode |> nodeToString, faultString |> nodeToString))

    member val Attachments = Dictionary<string, BinaryContent>() with get

    member this.RetrieveMessage() =
        use responseStream = response.GetResponseStream()
        responseStream.CopyTo(stream)

        endpoint.TriggerResponseReady(ResponseReadyEventArgs(this, request.Header, request.RequestId, methodMap.ServiceCode, methodMap.ServiceVersion |> Option.defaultValue ""))

        use content =
            stream.Position <- 0L
            let contentStream, atts = (stream, response) ||> MultipartMessage.read
            atts |> List.iter (fun content -> this.Attachments.Add(content.ContentID, content))
            contentStream

        content |> checkXRoadFault
        content.Position <- 0L
        use reader = XmlReader.Create(content)
        if not (reader.MoveToElement(0, "Envelope", XmlNamespace.SoapEnv)) then
            failwith "Soap envelope element was not found in response message."
        if not (reader.MoveToElement(1, "Body", XmlNamespace.SoapEnv)) then
            failwith "Soap body element was not found in response message."
        let context = SerializerContext(DefaultOffset=endpoint.DefaultOffset)
        this.Attachments |> Seq.iter (fun kvp -> context.AddAttachment(kvp.Key, kvp.Value, false))
        if not (reader.MoveToElement(2, null, null)) then
            failwith "Soap message has empty payload in response."
        // TODO : validate response wrapper element
        match reader.LocalName, reader.NamespaceURI with
        | "Fault", XmlNamespace.SoapEnv -> failwithf "Request resulted an error: %s" (reader.ReadInnerXml())
        | _ -> methodMap.Deserializer.Invoke(reader, context)

    interface IXRoadResponse with
        member _.Save(outputStream: Stream) =
            let headers = response.Headers.ToByteArray()
            outputStream.Write(headers, 0, headers.Length)
            stream.Position <- 0L
            stream.CopyTo(outputStream)

    interface IDisposable with
        member _.Dispose() =
            stream.Dispose()
            (response :> IDisposable).Dispose()

and internal XRoadRequest(endpoint: AbstractEndpointDeclaration, methodMap: MethodMap, header: XRoadHeader) =
    let request =
        let request = WebRequest.Create(endpoint.Uri, Method="POST", ContentType="text/xml; charset=utf-8") |> unbox<HttpWebRequest>
        request.Headers.Set("SOAPAction", "")
        if endpoint.AcceptedServerCertificate |> isNull |> not then
            request.ServerCertificateValidationCallback <-
                (fun _ cert _ errors -> if errors = SslPolicyErrors.None then true else cert = endpoint.AcceptedServerCertificate)
        endpoint.AuthenticationCertificates |> Seq.iter (request.ClientCertificates.Add >> ignore)
        request

    let addNamespace =
        let mutable i = 0
        (fun ns (writer: XmlWriter) ->
            if writer.LookupPrefix(ns) |> isNull then
                i <- i + 1
                writer.WriteAttributeString("xmlns", sprintf "ns%d" i, XmlNamespace.Xmlns, ns))

    let writeContent (stream: Stream) (content: Stream) =
        let buffer = Array.create 1000 0uy
        let rec writeChunk() =
            let bytesRead = content.Read(buffer, 0, 1000)
            stream.Write(buffer, 0, bytesRead)
            match bytesRead with 1000 -> writeChunk() | _ -> ()
        content.Position <- 0L
        writeChunk()

    let serializeMultipartMessage (context: SerializerContext) (serializeContent: Stream -> unit) (stream: Stream) =
        if context.Attachments.Count > 0 then
            let writer = new StreamWriter(stream, NewLine = "\r\n")
            let boundaryMarker = Guid.NewGuid().ToString()
            request.ContentType <-
                if context.IsMtomMessage then sprintf @"multipart/related; type=""application/xop+xml""; start=""<XML-%s>""; start-info=""text/xml""; boundary=""%s""" boundaryMarker boundaryMarker
                else sprintf @"multipart/related; type=""text/xml""; start=""<XML-%s>""; boundary=""%s""" boundaryMarker boundaryMarker
            request.Headers.Add("MIME-Version", "1.0")
            writer.WriteLine()
            writer.WriteLine("--{0}", boundaryMarker)
            if context.IsMtomMessage then writer.WriteLine(@"Content-Type: application/xop+xml; charset=UTF-8; type=""text/xml""")
            else writer.WriteLine("Content-Type: text/xml; charset=UTF-8")
            writer.WriteLine("Content-Transfer-Encoding: 8bit")
            writer.WriteLine("Content-ID: <XML-{0}>", boundaryMarker)
            writer.WriteLine()
            writer.Flush()
            stream |> serializeContent
            context.Attachments
            |> Seq.iter (fun kvp ->
                writer.WriteLine()
                writer.WriteLine("--{0}", boundaryMarker)
                writer.WriteLine("Content-Disposition: attachment; filename=notAnswering")
                writer.WriteLine("Content-Type: application/octet-stream")
                writer.WriteLine("Content-Transfer-Encoding: binary")
                writer.WriteLine("Content-ID: <{0}>", kvp.Key)
                writer.WriteLine()
                writer.Flush()
                use contentStream = kvp.Value.OpenStream()
                writeContent stream contentStream
                writer.WriteLine())
            writer.WriteLine("--{0}--", boundaryMarker)
            writer.Flush()
        else stream |> serializeContent

    let serializeMessage (context: SerializerContext) (content: Stream) =
        serializeMultipartMessage context (fun s -> writeContent s content)

    let writeStringHeader req ns (writer: XmlWriter) value name =
        if req |> Array.exists ((=) name) || value |> String.IsNullOrEmpty |> not then
            writer.WriteStartElement(name, ns)
            if methodMap.Request.IsEncoded then
                writer.WriteStartAttribute("type", XmlNamespace.Xsi)
                writer.WriteQualifiedName("string", XmlNamespace.Xsd)
                writer.WriteEndAttribute()
            if String.IsNullOrEmpty(value) |> not then
                writer.WriteValue(value)
            writer.WriteEndElement()

    let writeClientHeader (value: XRoadMemberIdentifier) req (writer: XmlWriter) =
        if req |> Array.exists ((=) "client") || not (value |> isNull) then
            writer.WriteStartElement("client", XmlNamespace.XRoad)
            if not (value |> isNull) then
                writer.WriteStartAttribute("objectType", XmlNamespace.XRoadIdentifiers)
                writer.WriteValue(value.ObjectId)
                writer.WriteEndAttribute()
                writer.WriteStartElement("xRoadInstance", XmlNamespace.XRoadIdentifiers)
                if not <| String.IsNullOrEmpty(value.XRoadInstance) then
                    writer.WriteValue(value.XRoadInstance)
                writer.WriteEndElement()
                writer.WriteStartElement("memberClass", XmlNamespace.XRoadIdentifiers)
                if not <| String.IsNullOrEmpty(value.MemberClass) then
                    writer.WriteValue(value.MemberClass)
                writer.WriteEndElement()
                writer.WriteStartElement("memberCode", XmlNamespace.XRoadIdentifiers)
                if not <| String.IsNullOrEmpty(value.MemberCode) then
                    writer.WriteValue(value.MemberCode)
                writer.WriteEndElement()
                if String.IsNullOrWhiteSpace(value.SubsystemCode) |> not then
                    writer.WriteStartElement("subsystemCode", XmlNamespace.XRoadIdentifiers)
                    writer.WriteValue(value.SubsystemCode)
                    writer.WriteEndElement()
            writer.WriteEndElement()

    let writeServiceHeader (value: XRoadMemberIdentifier) req (writer: XmlWriter) =
        if req |> Array.exists ((=) "service") || not (value |> isNull) then
            writer.WriteStartElement("service", XmlNamespace.XRoad)
            if not (value |> isNull) then
                writer.WriteStartAttribute("objectType", XmlNamespace.XRoadIdentifiers)
                writer.WriteValue(SERVICE_OBJECT_ID)
                writer.WriteEndAttribute()
                writer.WriteStartElement("xRoadInstance", XmlNamespace.XRoadIdentifiers)
                if not <| String.IsNullOrEmpty(value.XRoadInstance) then
                    writer.WriteValue(value.XRoadInstance)
                writer.WriteEndElement()
                writer.WriteStartElement("memberClass", XmlNamespace.XRoadIdentifiers)
                if not <| String.IsNullOrEmpty(value.MemberClass) then
                    writer.WriteValue(value.MemberClass)
                writer.WriteEndElement()
                writer.WriteStartElement("memberCode", XmlNamespace.XRoadIdentifiers)
                if not <| String.IsNullOrEmpty(value.MemberCode) then
                    writer.WriteValue(value.MemberCode)
                writer.WriteEndElement()
                if String.IsNullOrWhiteSpace(value.SubsystemCode) |> not then
                    writer.WriteStartElement("subsystemCode", XmlNamespace.XRoadIdentifiers)
                    writer.WriteValue(value.SubsystemCode)
                    writer.WriteEndElement()
                writer.WriteStartElement("serviceCode", XmlNamespace.XRoadIdentifiers)
                if not <| String.IsNullOrEmpty(methodMap.ServiceCode) then
                    writer.WriteValue(methodMap.ServiceCode)
                writer.WriteEndElement()
                match methodMap.ServiceVersion with
                | Some(version) ->
                    writer.WriteStartElement("serviceVersion", XmlNamespace.XRoadIdentifiers)
                    writer.WriteValue(version)
                    writer.WriteEndElement()
                | None -> ()
            writer.WriteEndElement()

    let writeXRoadHeader (id: string) (header: XRoadHeader) (writer: XmlWriter) =
        methodMap.RequiredHeaders.Keys |> Seq.iter (fun ns -> writer |> addNamespace ns)
        let requiredHeaders = match methodMap.RequiredHeaders.TryGetValue(XmlNamespace.XRoad) with true, xs -> xs | _ -> [||]
        let writeStringHeader' = writeStringHeader requiredHeaders XmlNamespace.XRoad writer
        if writer.LookupPrefix(XmlNamespace.XRoadIdentifiers) |> isNull then
            writer.WriteAttributeString("xmlns", "id", XmlNamespace.Xmlns, XmlNamespace.XRoadIdentifiers)
        writer |> writeClientHeader header.Client requiredHeaders
        writer |> writeServiceHeader header.Producer requiredHeaders
        writeStringHeader' id "id"
        writeStringHeader' header.UserId "userId"
        writeStringHeader' header.Issue "issue"
        writeStringHeader' header.ProtocolVersion "protocolVersion"
        header.Unresolved |> Seq.iter (fun e -> e.WriteTo(writer))

    let stream = new MemoryStream() :> Stream

    member val RequestId = if String.IsNullOrWhiteSpace(header.Id) then getUUID() else header.Id with get
    member val Header = header with get

    member this.CreateMessage(args) =
        use content = new MemoryStream()
        use sw = new StreamWriter(content)
        let context = SerializerContext(DefaultOffset=endpoint.DefaultOffset, IsMultipart = methodMap.Request.IsMultipart)
        use writer = XmlWriter.Create(sw)
        writer.WriteStartDocument()
        writer.WriteStartElement("soapenv", "Envelope", XmlNamespace.SoapEnv)
        writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
        writer.WriteAttributeString("xmlns", "xro", XmlNamespace.Xmlns, XmlNamespace.XRoad)
        methodMap.Namespaces |> Seq.iteri (fun i ns -> writer.WriteAttributeString("xmlns", sprintf "ns%d" i, XmlNamespace.Xmlns, ns))
        methodMap.Request.Accessor |> Option.iter (fun acc -> writer.WriteAttributeString("xmlns", "acc", XmlNamespace.Xmlns, acc.Namespace))
        if methodMap.Request.IsEncoded then
            writer.WriteAttributeString("xmlns", "xsd", XmlNamespace.Xmlns, XmlNamespace.Xsd)
            writer.WriteAttributeString("encodingStyle", XmlNamespace.SoapEnv, XmlNamespace.SoapEnc)
        writer.WriteStartElement("Header", XmlNamespace.SoapEnv)
        writer |> writeXRoadHeader this.RequestId header
        writer.WriteEndElement()
        writer.WriteStartElement("Body", XmlNamespace.SoapEnv)
        methodMap.Serializer.Invoke(writer, args, context)
        writer.WriteEndElement()
        writer.WriteEndDocument()
        writer.Flush()
        (content, stream) ||> serializeMessage context
        endpoint.TriggerRequestReady(RequestReadyEventArgs(this, header, this.RequestId, methodMap.ServiceCode, methodMap.ServiceVersion |> Option.defaultValue ""))

    member _.SendMessage() =
        stream.Position <- 0L
        use networkStream = request.GetRequestStream()
        stream.CopyTo(networkStream)

    member _.GetResponse() =
        request.GetResponse()

    interface IXRoadRequest with
        member _.Save(outputStream: Stream) =
            let headers = request.Headers.ToByteArray()
            outputStream.Write(headers, 0, headers.Length)
            stream.Position <- 0L
            stream.CopyTo(outputStream)
        member _.HttpWebRequest with get() = request

    interface IDisposable with
        member _.Dispose() =
            stream.Dispose()

type public XRoadUtil =
    static member MakeServiceCall(endpoint: AbstractEndpointDeclaration, methodName: string, header: XRoadHeader, args: obj[]) =
        let serviceMethod = endpoint.GetType().GetMethod(methodName, BindingFlags.Instance ||| BindingFlags.DeclaredOnly ||| BindingFlags.NonPublic ||| BindingFlags.Public)
        let serviceMethodMap = getMethodMap serviceMethod
        use request = new XRoadRequest(endpoint, serviceMethodMap, header)
        request.CreateMessage(args)
        request.SendMessage()
        use response = new XRoadResponse(endpoint, request, serviceMethodMap)
        let result = response.RetrieveMessage()
        if serviceMethod.ReturnType.IsGenericType && serviceMethod.ReturnType.GetGenericTypeDefinition() = typedefof<MultipartResponse<_>> then
            Activator.CreateInstance(serviceMethod.ReturnType, [| box result; response.Attachments |> Seq.map (fun kvp -> kvp.Value) |> box |])
        else result
