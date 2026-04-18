namespace FSharp.Data.XRoad.Protocol

open System.Net.Http
open System.Net.Http.Headers
open System.Threading
open FSharp.Data.XRoad
open FSharp.Data.XRoad.Emit
open FSharp.Data.XRoad.Extensions
open NodaTime
open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Security
open System.Reflection
open System.Text
open System.Xml
open System.Xml.XPath

type internal XRoadFault(faultCode: string, faultString) =
    inherit Exception(faultString)
    member val FaultCode = faultCode with get
    member val FaultString = faultString with get

module internal XRoadMessage =
    let [<Literal>] FAULT_PATH = "/*[local-name()='Envelope' and namespace-uri()='http://schemas.xmlsoap.org/soap/envelope/']/*[local-name()='Body' and namespace-uri()='http://schemas.xmlsoap.org/soap/envelope/']/*[faultCode|faultString]"
    let [<Literal>] SOAP_ENVELOPE_CONTENT_ID = @"""<Envelope@Soap>"""

    let utf8WithoutBom = UTF8Encoding(false)

    let parseSoapEnvelopeBody (stream: Stream) : XmlReader =
        let reader = XmlReader.Create(stream, XmlReaderSettings(CloseInput = false))
        if not (reader.MoveToElement(0, "Envelope", XmlNamespace.SoapEnv)) then
            failwith "Soap envelope element was not found in response message."
        if not (reader.MoveToElement(1, "Body", XmlNamespace.SoapEnv)) then
            failwith "Soap body element was not found in response message."
        reader

    let checkFaultInStream (stream: Stream) : unit =
        stream.Position <- 0L
        use reader = XmlReader.Create(stream, XmlReaderSettings(CloseInput = false))
        let doc = XPathDocument(reader)
        let nav = doc.CreateNavigator()
        match nav.SelectSingleNode(FAULT_PATH) with
        | null -> ()
        | node ->
            let faultCode = node.SelectSingleNode("./faultCode")
            let faultString = node.SelectSingleNode("./faultString")
            let nodeToString = Option.ofObj >> Option.map (fun x -> (x: XPathNavigator).Value) >> Option.defaultValue ""
            raise (XRoadFault(faultCode |> nodeToString, faultString |> nodeToString))

    let createNamespaceBuilder () =
        let mutable i = 0
        (fun ns (writer: XmlWriter) ->
            if writer.LookupPrefix(ns) |> isNull then
                i <- i + 1
                writer.WriteAttributeString("xmlns", $"ns%d{i}", XmlNamespace.Xmlns, ns))

    let writeStringHeader isEncoded req ns (writer: XmlWriter) value name =
        if req |> Array.exists ((=) name) || value |> String.IsNullOrEmpty |> not then
            writer.WriteStartElement(name, ns)
            if isEncoded then
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

    let writeServiceHeader (serviceCode: string, serviceVersion: string option) (value: XRoadMemberIdentifier) req (writer: XmlWriter) =
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
                if not <| String.IsNullOrEmpty(serviceCode) then
                    writer.WriteValue(serviceCode)
                writer.WriteEndElement()
                match serviceVersion with
                | Some(version) ->
                    writer.WriteStartElement("serviceVersion", XmlNamespace.XRoadIdentifiers)
                    writer.WriteValue(version)
                    writer.WriteEndElement()
                | None -> ()
            writer.WriteEndElement()

    let create (header: XRoadHeader) (serializerContext: SerializerContext) (args: obj array) (methodMap: MethodMap) =
        let requestId = if String.IsNullOrWhiteSpace(header.Id) then getUUID() else header.Id

        let addNamespace = createNamespaceBuilder()

        let writeXRoadHeader (id: string) (header: XRoadHeader) (writer: XmlWriter) =
            methodMap.RequiredHeaders.Keys |> Seq.iter (fun ns -> writer |> addNamespace ns)
            let requiredHeaders = match methodMap.RequiredHeaders.TryGetValue(XmlNamespace.XRoad) with true, xs -> xs | _ -> [||]
            let writeStringHeader' = writeStringHeader methodMap.Request.IsEncoded requiredHeaders XmlNamespace.XRoad writer
            if writer.LookupPrefix(XmlNamespace.XRoadIdentifiers) |> isNull then
                writer.WriteAttributeString("xmlns", "id", XmlNamespace.Xmlns, XmlNamespace.XRoadIdentifiers)
            writer |> writeClientHeader header.Client requiredHeaders
            writer |> writeServiceHeader (methodMap.ServiceCode, methodMap.ServiceVersion) header.Producer requiredHeaders
            writeStringHeader' id "id"
            writeStringHeader' header.UserId "userId"
            writeStringHeader' header.Issue "issue"
            writeStringHeader' header.ProtocolVersion "protocolVersion"
            header.Unresolved |> Seq.iter _.WriteTo(writer)

        let envelope = new MemoryStream()
        try
            use envelopeWriter = new StreamWriter(envelope, utf8WithoutBom, 1024, leaveOpen = true)
            use writer = XmlWriter.Create(envelopeWriter, XmlWriterSettings(CloseOutput = false))
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
            writer |> writeXRoadHeader requestId header
            writer.WriteEndElement()
            writer.WriteStartElement("Body", XmlNamespace.SoapEnv)
            methodMap.Serializer.Invoke(writer, args, serializerContext)
            writer.WriteEndElement()
            writer.WriteEndDocument()
        with _ ->
            envelope.Dispose()
            reraise()

        envelope.Seek(0L, SeekOrigin.Begin) |> ignore

        let httpContent: HttpContent =
            if serializerContext.Attachments.Count > 0
            then new MultipartContent("related")
            else new StreamContent(envelope)

        try
            match httpContent with
            | :? MultipartContent as multipartContent ->
                if serializerContext.IsMtomMessage then
                    multipartContent.Headers.ContentType.Parameters.Add(NameValueHeaderValue("type", @"""application/xop+xml"""))
                    multipartContent.Headers.ContentType.Parameters.Add(NameValueHeaderValue("start", SOAP_ENVELOPE_CONTENT_ID))
                    multipartContent.Headers.ContentType.Parameters.Add(NameValueHeaderValue("start-info", @"""text/xml"""))
                else
                    multipartContent.Headers.ContentType.Parameters.Add(NameValueHeaderValue("type", @"""text/xml"""))
                    multipartContent.Headers.ContentType.Parameters.Add(NameValueHeaderValue("start", SOAP_ENVELOPE_CONTENT_ID))

                let soapContent = new StreamContent(envelope)
                multipartContent.Add(soapContent)
                if serializerContext.IsMtomMessage then
                    soapContent.Headers.ContentType <- MediaTypeHeaderValue("application/xop+xml", CharSet = "UTF-8")
                    soapContent.Headers.ContentType.CharSet <- "UTF-8"
                    soapContent.Headers.ContentType.Parameters.Add(NameValueHeaderValue("type", @"""text/xml"""))
                else
                    soapContent.Headers.ContentType <- MediaTypeHeaderValue("text/xml")
                    soapContent.Headers.ContentType.CharSet <- "UTF-8"
                soapContent.Headers.TryAddWithoutValidation("Content-Transfer-Encoding", "8bit") |> ignore
                soapContent.Headers.TryAddWithoutValidation("Content-ID", SOAP_ENVELOPE_CONTENT_ID.Trim('"')) |> ignore

                serializerContext.Attachments
                |> Seq.iter (fun kvp ->
                    let attachmentContent = new StreamContent(kvp.Value.OpenStream())
                    multipartContent.Add(attachmentContent)
                    attachmentContent.Headers.ContentDisposition <- ContentDispositionHeaderValue("attachment")
                    attachmentContent.Headers.ContentDisposition.FileName <- "notAnswering"
                    attachmentContent.Headers.ContentType <- MediaTypeHeaderValue("application/octet-stream")
                    attachmentContent.Headers.TryAddWithoutValidation("Content-Transfer-Encoding", "binary") |> ignore
                    attachmentContent.Headers.TryAddWithoutValidation("Content-ID", $"<%s{kvp.Key}>") |> ignore
                )
            | :? StreamContent as streamContent ->
                streamContent.Headers.ContentType <- MediaTypeHeaderValue("text/xml")
                streamContent.Headers.ContentType.CharSet <- "UTF-8"
            | _ -> failwith "never"
        with _ ->
            httpContent.Dispose()
            reraise()

        httpContent

type internal XRoadResponse(endpoint: AbstractEndpointDeclaration, request: XRoadRequest, methodMap: MethodMap) =
    let response: WebResponse = request.GetResponse()
    let stream = new MemoryStream()

    let checkXRoadFault = XRoadMessage.checkFaultInStream

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
        use reader = XRoadMessage.parseSoapEnvelopeBody content
        let context = SerializerContext(DefaultOffset=endpoint.DefaultOffset)
        this.Attachments |> Seq.iter (fun kvp -> context.AddAttachment(kvp.Key, kvp.Value, false))
        if not (reader.MoveToElement(2, null, null)) then
            failwith "Soap message has empty payload in response."
        // TODO : validate response wrapper element
        match reader.LocalName, reader.NamespaceURI with
        | "Fault", XmlNamespace.SoapEnv -> raise (XRoadFault("", reader.ReadInnerXml()))
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
    let requestId = if String.IsNullOrWhiteSpace(header.Id) then getUUID() else header.Id
    let request =
        let request = WebRequest.Create(endpoint.Uri, Method="POST", ContentType="text/xml; charset=utf-8") |> unbox<HttpWebRequest>
        request.Headers.Set("SOAPAction", "")
        request.Timeout <- endpoint.Timeout
        if endpoint.AcceptedServerCertificate |> isNull |> not then
            request.ServerCertificateValidationCallback <-
                (fun _ cert _ errors -> if errors = SslPolicyErrors.None then true else cert = endpoint.AcceptedServerCertificate)
        endpoint.AuthenticationCertificates |> Seq.iter (request.ClientCertificates.Add >> ignore)
        request

    let addNamespace = XRoadMessage.createNamespaceBuilder()

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
            use writer = new StreamWriter(stream, XRoadMessage.utf8WithoutBom, 1024, leaveOpen = true)
            writer.NewLine <- "\r\n"
            let boundaryMarker = Guid.NewGuid().ToString()
            request.ContentType <-
                if context.IsMtomMessage then $@"multipart/related; type=""application/xop+xml""; start=""<XML-%s{boundaryMarker}>""; start-info=""text/xml""; boundary=""%s{boundaryMarker}"""
                else $@"multipart/related; type=""text/xml""; start=""<XML-%s{boundaryMarker}>""; boundary=""%s{boundaryMarker}"""
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

    let writeXRoadHeader (id: string) (header: XRoadHeader) (writer: XmlWriter) =
        methodMap.RequiredHeaders.Keys |> Seq.iter (fun ns -> writer |> addNamespace ns)
        let requiredHeaders = match methodMap.RequiredHeaders.TryGetValue(XmlNamespace.XRoad) with true, xs -> xs | _ -> [||]
        let writeStringHeader' = XRoadMessage.writeStringHeader methodMap.Request.IsEncoded requiredHeaders XmlNamespace.XRoad writer
        if writer.LookupPrefix(XmlNamespace.XRoadIdentifiers) |> isNull then
            writer.WriteAttributeString("xmlns", "id", XmlNamespace.Xmlns, XmlNamespace.XRoadIdentifiers)
        writer |> XRoadMessage.writeClientHeader header.Client requiredHeaders
        writer |> XRoadMessage.writeServiceHeader (methodMap.ServiceCode, methodMap.ServiceVersion) header.Producer requiredHeaders
        writeStringHeader' id "id"
        writeStringHeader' header.UserId "userId"
        writeStringHeader' header.Issue "issue"
        writeStringHeader' header.ProtocolVersion "protocolVersion"
        header.Unresolved |> Seq.iter _.WriteTo(writer)

    let stream = new MemoryStream() :> Stream

    member _.RequestId with get() = requestId
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
        methodMap.Namespaces |> Seq.iteri (fun i ns -> writer.WriteAttributeString("xmlns", $"ns%d{i}", XmlNamespace.Xmlns, ns))
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
        member _.RequestId with get() = requestId
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
    static let toResult (methodInfo: MethodInfo) (response: XRoadResponse) =
        let result = response.RetrieveMessage()
        let returnType = methodInfo.ReturnType
        if returnType.IsGenericType && returnType.GetGenericTypeDefinition() = typedefof<MultipartResponse<_>> then
            Activator.CreateInstance(returnType, [| box result; response.Attachments |> Seq.map _.Value |> box |])
        else result

    static member GetServiceMethod(serviceTy: Type, name: string) =
        serviceTy.GetMethod(name, BindingFlags.Instance ||| BindingFlags.DeclaredOnly ||| BindingFlags.NonPublic ||| BindingFlags.Public)

    static member MakeServiceCallAsync(httpClient: HttpClient, defaultOffset: Offset, header: XRoadHeader, methodInfo: MethodInfo, args: obj[], ct: CancellationToken) =
        task {
            let methodMap = getMethodMap methodInfo
            let serializerContext = SerializerContext(DefaultOffset = defaultOffset, IsMultipart = methodMap.Request.IsMultipart, CancellationToken = ct)
            use httpRequestMessage = new HttpRequestMessage(HttpMethod.Post, "")
            httpRequestMessage.Headers.Add("SOAPAction", "")
            httpRequestMessage.Content <- XRoadMessage.create header serializerContext args methodMap
            if httpRequestMessage.Content :? MultipartContent then
                httpRequestMessage.Headers.Add("MIME-Version", "1.0")
            use! httpResponseMessage = httpClient.SendAsync(httpRequestMessage, ct)
            httpResponseMessage.EnsureSuccessStatusCode() |> ignore
            let response = Unchecked.defaultof<XRoadResponse> // TODO
            return response |> toResult methodInfo
        }

    static member MakeServiceCall_Legacy(endpoint: AbstractEndpointDeclaration, methodName: string, header: XRoadHeader, args: obj[]) =
        let serviceMethod = XRoadUtil.GetServiceMethod(endpoint.GetType(), methodName)
        let serviceMethodMap = getMethodMap serviceMethod
        use request = new XRoadRequest(endpoint, serviceMethodMap, header)
        request.CreateMessage(args)
        request.SendMessage()
        use response = new XRoadResponse(endpoint, request, serviceMethodMap)
        response |> toResult serviceMethod
