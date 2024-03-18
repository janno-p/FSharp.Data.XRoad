namespace FSharp.Data.XRoad.Protocol

open FSharp.Data.XRoad
open FSharp.Data.XRoad.Emit
open FSharp.Data.XRoad.Extensions
open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Reflection
open System.Xml
open System.Xml.XPath

type internal XRoadFault(faultCode: string, faultString) =
    inherit Exception(faultString)
    member val FaultCode = faultCode with get
    member val FaultString = faultString with get

[<RequireQualifiedAccess>]
module internal XRoadMessageReader =
    [<Literal>]
    let private FAULT_PATH = "/*[local-name()='Envelope' and namespace-uri()='http://schemas.xmlsoap.org/soap/envelope/']/*[local-name()='Body' and namespace-uri()='http://schemas.xmlsoap.org/soap/envelope/']/*[faultCode|faultString]"

    let private checkXRoadFault (stream: Stream) =
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

    let deserializeResponse (methodMap: MethodMap) (context: SerializerContext) (response: HttpResponseMessage) =
        let attachments = Dictionary<string, BinaryContent>()
        use content =
            // TODO: Use MultipartReader
            match response.Content with
            | :? MultipartContent as multipartContent ->
                multipartContent
                |> Seq.skip 1
                |> Seq.iter (fun x ->
                    let contentId = x.Headers.GetValues("Content-ID").ToString()
                    let binaryContent = BinaryContent.Create(contentId, x.ReadAsByteArrayAsync() |> Async.AwaitTask |> Async.RunSynchronously)
                    attachments.Add(contentId, binaryContent))
                let x = multipartContent |> Seq.head
                x.ReadAsStreamAsync() |> Async.AwaitTask |> Async.RunSynchronously
            | _ ->
                response.Content.ReadAsStreamAsync() |> Async.AwaitTask |> Async.RunSynchronously
        content |> checkXRoadFault
        content.Position <- 0L
        use reader = XmlReader.Create(content)
        if not (reader.MoveToElement(0, "Envelope", XmlNamespace.SoapEnv)) then
            failwith "Soap envelope element was not found in response message."
        if not (reader.MoveToElement(1, "Body", XmlNamespace.SoapEnv)) then
            failwith "Soap body element was not found in response message."
        attachments |> Seq.iter (fun kvp -> context.AddAttachment(kvp.Key, kvp.Value, false))
        if not (reader.MoveToElement(2, null, null)) then
            failwith "Soap message has empty payload in response."
        // TODO : validate response wrapper element
        match reader.LocalName, reader.NamespaceURI with
        | "Fault", XmlNamespace.SoapEnv -> failwith $"Request resulted an error: %s{reader.ReadInnerXml()}"
        | _ -> methodMap.Deserializer.Invoke(reader, context)

[<RequireQualifiedAccess>]
module internal XRoadMessageWriter =
    type private AddNamespace = string -> XmlWriter -> unit

    let private createAddNamespace () : AddNamespace =
        let mutable i = 0
        (fun ns (writer: XmlWriter) ->
            if writer.LookupPrefix(ns) |> isNull then
                i <- i + 1
                writer.WriteAttributeString("xmlns", $"ns%d{i}", XmlNamespace.Xmlns, ns))

    let private writeStringHeader (methodMap: MethodMap) req ns (writer: XmlWriter) value name =
        if req |> Array.exists ((=) name) || value |> String.IsNullOrEmpty |> not then
            writer.WriteStartElement(name, ns)
            if methodMap.Request.IsEncoded then
                writer.WriteStartAttribute("type", XmlNamespace.Xsi)
                writer.WriteQualifiedName("string", XmlNamespace.Xsd)
                writer.WriteEndAttribute()
            if String.IsNullOrEmpty(value) |> not then
                writer.WriteValue(value)
            writer.WriteEndElement()

    let private writeClientHeader (value: XRoadMemberIdentifier) req (writer: XmlWriter) =
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

    let private writeServiceHeader (methodMap: MethodMap) (value: XRoadMemberIdentifier) req (writer: XmlWriter) =
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

    let private writeXRoadHeader (methodMap: MethodMap) (addNamespace: AddNamespace) (id: string) (header: XRoadHeader) (writer: XmlWriter) =
        methodMap.RequiredHeaders.Keys |> Seq.iter (fun ns -> writer |> addNamespace ns)
        let requiredHeaders = match methodMap.RequiredHeaders.TryGetValue(XmlNamespace.XRoad) with true, xs -> xs | _ -> [||]
        let writeStringHeader' = writeStringHeader methodMap requiredHeaders XmlNamespace.XRoad writer
        if writer.LookupPrefix(XmlNamespace.XRoadIdentifiers) |> isNull then
            writer.WriteAttributeString("xmlns", "id", XmlNamespace.Xmlns, XmlNamespace.XRoadIdentifiers)
        writer |> writeClientHeader header.Client requiredHeaders
        writer |> writeServiceHeader methodMap header.Producer requiredHeaders
        writeStringHeader' id "id"
        writeStringHeader' header.UserId "userId"
        writeStringHeader' header.Issue "issue"
        writeStringHeader' header.ProtocolVersion "protocolVersion"
        header.Unresolved |> Seq.iter _.WriteTo(writer)

    let private serializeMessage (context: SerializerContext) (stream: MemoryStream) : HttpContent =
        if context.Attachments.Count > 0 then
            let boundaryMarker = Guid.NewGuid().ToString()
            let content = new MultipartContent("related", boundaryMarker)
            stream.Seek(0L, SeekOrigin.Begin) |> ignore
            let xmlContent = new StreamContent(stream)
            content.Add(xmlContent)
            if context.IsMtomMessage then
                content.Headers.ContentType.Parameters.Add(NameValueHeaderValue("type", @"""application/xop+xml"""))
                content.Headers.ContentType.Parameters.Add(NameValueHeaderValue("start-info", @"""text/xml"""))
                xmlContent.Headers.ContentType <- MediaTypeHeaderValue("application/xop+xml")
                xmlContent.Headers.ContentType.CharSet <- "UTF-8"
                xmlContent.Headers.ContentType.Parameters.Add(NameValueHeaderValue("type", @"""text/xml"""))
            else
                content.Headers.ContentType.Parameters.Add(NameValueHeaderValue("type", @"""text/xml"""))
                xmlContent.Headers.ContentType <- MediaTypeHeaderValue("text/xml")
                xmlContent.Headers.ContentType.CharSet <- "UTF-8"
            content.Headers.ContentType.Parameters.Add(NameValueHeaderValue("start", $@"""<XML-%s{boundaryMarker}>"""))
            xmlContent.Headers.TryAddWithoutValidation("Content-Transfer-Encoding", "8bit") |> ignore
            xmlContent.Headers.TryAddWithoutValidation("Content-ID", $"<XML-%s{boundaryMarker}>") |> ignore
            context.Attachments
            |> Seq.iter (fun kvp ->
                let attachmentStream = new StreamContent(kvp.Value.OpenStream())
                attachmentStream.Headers.ContentDisposition <- ContentDispositionHeaderValue("attachment")
                attachmentStream.Headers.ContentDisposition.FileName <- "notAnswering"
                attachmentStream.Headers.ContentType <- MediaTypeHeaderValue("application/octet-stream")
                attachmentStream.Headers.TryAddWithoutValidation("Content-Transfer-Encoding", "binary") |> ignore
                attachmentStream.Headers.TryAddWithoutValidation("Content-ID", $"<%s{kvp.Key}>") |> ignore
                content.Add(attachmentStream))
            content
        else
            stream.Seek(0L, SeekOrigin.Begin) |> ignore
            let content = new StreamContent(stream)
            content.Headers.ContentType <- MediaTypeHeaderValue("text/xml")
            content.Headers.ContentType.CharSet <- "utf-8"
            content

    let createHttpContent (methodMap: MethodMap) (context: SerializerContext) (header: XRoadHeader) args =
        let addNamespace = createAddNamespace()
        let requestId = if String.IsNullOrWhiteSpace(header.Id) then getUUID() else header.Id
        let stream = new MemoryStream()
        use sw = new StreamWriter(stream, utf8WithoutBom, 1024, true)
        use writer = XmlWriter.Create(sw)
        writer.WriteStartDocument()
        writer.WriteStartElement("soapenv", "Envelope", XmlNamespace.SoapEnv)
        writer.WriteAttributeString("xmlns", "xsi", XmlNamespace.Xmlns, XmlNamespace.Xsi)
        writer.WriteAttributeString("xmlns", "xro", XmlNamespace.Xmlns, XmlNamespace.XRoad)
        methodMap.Namespaces |> Seq.iteri (fun i ns -> writer.WriteAttributeString("xmlns", $"ns%d{i}", XmlNamespace.Xmlns, ns))
        methodMap.Request.Accessor |> Option.iter (fun acc -> writer.WriteAttributeString("xmlns", "acc", XmlNamespace.Xmlns, acc.Namespace))
        if methodMap.Request.IsEncoded then
            writer.WriteAttributeString("xmlns", "xsd", XmlNamespace.Xmlns, XmlNamespace.Xsd)
            writer.WriteAttributeString("encodingStyle", XmlNamespace.SoapEnv, XmlNamespace.SoapEnc)
        writer.WriteStartElement("Header", XmlNamespace.SoapEnv)
        writer |> writeXRoadHeader methodMap addNamespace requestId header
        writer.WriteEndElement()
        writer.WriteStartElement("Body", XmlNamespace.SoapEnv)
        methodMap.Serializer.Invoke(writer, args, context)
        writer.WriteEndElement()
        writer.WriteEndDocument()
        writer.Flush()
        requestId, serializeMessage context stream

type public XRoadUtil =
    static member MakeServiceCall(endpoint: AbstractEndpointDeclaration, methodName: string, header: XRoadHeader, args: obj[]) =
        let serviceMethod = endpoint.GetType().GetMethod(methodName, BindingFlags.Instance ||| BindingFlags.DeclaredOnly ||| BindingFlags.NonPublic ||| BindingFlags.Public)
        let methodMap = getMethodMap serviceMethod
        use httpRequestMessage = new HttpRequestMessage(HttpMethod.Post, "")
        httpRequestMessage.Headers.TryAddWithoutValidation("SOAPAction", "") |> ignore
        let requestContext = SerializerContext(DefaultOffset = endpoint.DefaultOffset, IsMultipart = methodMap.Request.IsMultipart)
        let requestId, httpContent = XRoadMessageWriter.createHttpContent methodMap requestContext header args
        httpRequestMessage.Content <- httpContent
        if httpContent :? MultipartContent then
            httpRequestMessage.Headers.TryAddWithoutValidation("MIME-Version", "1.0") |> ignore
        endpoint.TriggerRequestReady(RequestReadyEventArgs(httpRequestMessage, header, requestId, methodMap.ServiceCode, methodMap.ServiceVersion |> Option.defaultValue ""))
        use httpResponseMessage = endpoint.HttpClient.SendAsync(httpRequestMessage) |> Async.AwaitTask |> Async.RunSynchronously
        endpoint.TriggerResponseReady(ResponseReadyEventArgs(httpResponseMessage, header, requestId, methodMap.ServiceCode, methodMap.ServiceVersion |> Option.defaultValue ""))
        let responseContext = SerializerContext(DefaultOffset = endpoint.DefaultOffset)
        let result = httpResponseMessage |> XRoadMessageReader.deserializeResponse methodMap responseContext
        if serviceMethod.ReturnType.IsGenericType && serviceMethod.ReturnType.GetGenericTypeDefinition() = typedefof<MultipartResponse<_>> then
            Activator.CreateInstance(serviceMethod.ReturnType, [| box result; responseContext.Attachments |> Seq.map _.Value |> box |])
        else result
