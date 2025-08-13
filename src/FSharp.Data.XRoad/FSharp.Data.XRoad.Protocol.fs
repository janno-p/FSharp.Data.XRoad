namespace FSharp.Data.XRoad.Protocol

open System.Net.Http.Headers
open FSharp.Data.XRoad
open FSharp.Data.XRoad.Emit
open FSharp.Data.XRoad.Extensions
open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Reflection
open System.Threading
open System.Threading.Tasks
open System.Xml
open System.Xml.XPath

[<AutoOpen>]
module internal Helpers =
    let [<Literal>] FAULT_PATH = "/*[local-name()='Envelope' and namespace-uri()='http://schemas.xmlsoap.org/soap/envelope/']/*[local-name()='Body' and namespace-uri()='http://schemas.xmlsoap.org/soap/envelope/']/*[faultCode|faultString]"

    let utf8WithoutBom = System.Text.UTF8Encoding(false, true)

type internal XRoadFault(faultCode: string, faultString) =
    inherit Exception(faultString)
    member val FaultCode = faultCode with get
    member val FaultString = faultString with get

type internal XRoadHttpResponse(endpoint: AbstractEndpointDeclaration, methodMap: MethodMap, header: XRoadHeader, httpResponseMessage: HttpResponseMessage) =
    let checkXRoadFault (stream: MemoryStream) =
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

    member this.DeserializeMessage() =
        task {
            do! httpResponseMessage.Content.LoadIntoBufferAsync()

            do! endpoint.TriggerResponseReady(
                ResponseReadyEventArgs(
                    httpResponseMessage,
                    header,
                    methodMap.ServiceCode,
                    methodMap.ServiceVersion |> Option.defaultValue ""
                )
            )

            use! stream = httpResponseMessage.Content.ReadAsStreamAsync()

            use content =
                stream.Position <- 0L
                let contentType = Some httpResponseMessage.Content.Headers.ContentType
                let contentStream, atts = (stream, contentType) ||> MultipartMessage.read
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
            return
                // TODO : validate response wrapper element
                match reader.LocalName, reader.NamespaceURI with
                | "Fault", XmlNamespace.SoapEnv -> failwith $"Request resulted an error: %s{reader.ReadInnerXml()}"
                | _ -> methodMap.Deserializer.Invoke(reader, context)
        }

and internal XRoadHttpRequest(
    endpoint: AbstractEndpointDeclaration,
    methodMap: MethodMap,
    header: XRoadHeader,
    httpRequestMessage: HttpRequestMessage
    ) =
    do
        // content-type: text/xml; charset=utf-8
        httpRequestMessage.Method <- HttpMethod.Post
        httpRequestMessage.Headers.TryAddWithoutValidation("SOAPAction", "") |> ignore

    let addNamespace =
        let mutable i = 0
        (fun ns (writer: XmlWriter) ->
            if writer.LookupPrefix(ns) |> isNull then
                i <- i + 1
                writer.WriteAttributeString("xmlns", $"ns%d{i}", XmlNamespace.Xmlns, ns))

    let serializeMessage (context: SerializerContext) (contentStream: MemoryStream) =
        if context.Attachments.Count > 0 then
            let boundaryMarker = Guid.NewGuid().ToString()
            let multipartContent = new MultipartContent("related", boundaryMarker)
            multipartContent.Headers.TryAddWithoutValidation("MIME-Version", "1.0") |> ignore
            multipartContent.Headers.ContentType.Parameters.Add(NameValueHeaderValue("start", $"<XML-%s{boundaryMarker}>"))
            if context.IsMtomMessage then
                multipartContent.Headers.ContentType.Parameters.Add(NameValueHeaderValue("type", "application/xop+xml"))
                multipartContent.Headers.ContentType.Parameters.Add(NameValueHeaderValue("start-info", "text/xml"))
            else
                multipartContent.Headers.ContentType.Parameters.Add(NameValueHeaderValue("type", "text/xml"))
            let xmlContent = new StreamContent(contentStream)
            xmlContent.Headers.ContentType <-
                if context.IsMtomMessage then
                    let contentType = MediaTypeHeaderValue("application/xop+xml", CharSet = "UTF-8")
                    contentType.Parameters.Add(NameValueHeaderValue("type", "text/xml"))
                    contentType
                else MediaTypeHeaderValue("text/xml", CharSet = "UTF-8")
            xmlContent.Headers.TryAddWithoutValidation("Content-Transfer-Encoding", "8bit") |> ignore
            xmlContent.Headers.TryAddWithoutValidation("Content-ID", $"<XML-%s{boundaryMarker}") |> ignore
            multipartContent.Add(xmlContent)
            context.Attachments
            |> Seq.iter (fun kvp ->
                let attachmentContent = new StreamContent(kvp.Value.OpenStream())
                attachmentContent.Headers.ContentDisposition <- ContentDispositionHeaderValue("attachment", FileName = "notAnswering")
                attachmentContent.Headers.ContentType <- MediaTypeHeaderValue("application/octet-stream")
                attachmentContent.Headers.TryAddWithoutValidation("Content-Transfer-Encoding", "binary") |> ignore
                attachmentContent.Headers.TryAddWithoutValidation("Content-ID", $"<%s{kvp.Key}>") |> ignore
                multipartContent.Add(attachmentContent)
            )
            httpRequestMessage.Content <- multipartContent
        else
            httpRequestMessage.Content <- new StreamContent(contentStream)
            httpRequestMessage.Content.Headers.ContentType <- MediaTypeHeaderValue("text/xml", CharSet = "UTF-8")

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

    let writeXRoadHeader (header: XRoadHeader) (writer: XmlWriter) =
        methodMap.RequiredHeaders.Keys |> Seq.iter (fun ns -> writer |> addNamespace ns)
        let requiredHeaders = match methodMap.RequiredHeaders.TryGetValue(XmlNamespace.XRoad) with true, xs -> xs | _ -> [||]
        let writeStringHeader' = writeStringHeader requiredHeaders XmlNamespace.XRoad writer
        if writer.LookupPrefix(XmlNamespace.XRoadIdentifiers) |> isNull then
            writer.WriteAttributeString("xmlns", "id", XmlNamespace.Xmlns, XmlNamespace.XRoadIdentifiers)
        writer |> writeClientHeader header.Client requiredHeaders
        writer |> writeServiceHeader header.Producer requiredHeaders
        writeStringHeader' header.Id "id"
        writeStringHeader' header.UserId "userId"
        writeStringHeader' header.Issue "issue"
        writeStringHeader' header.ProtocolVersion "protocolVersion"
        header.Unresolved |> Seq.iter _.WriteTo(writer)

    member val Header = header with get

    member this.SerializeMessage(args) : Task =
        task {
            let content = new MemoryStream()
            use sw = new StreamWriter(content, utf8WithoutBom, 1024, true)

            let context =
                SerializerContext(
                    DefaultOffset = endpoint.DefaultOffset,
                    IsMultipart = methodMap.Request.IsMultipart
                )

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
            writer |> writeXRoadHeader header
            writer.WriteEndElement()
            writer.WriteStartElement("Body", XmlNamespace.SoapEnv)
            methodMap.Serializer.Invoke(writer, args, context)
            writer.WriteEndElement()
            writer.WriteEndDocument()
            writer.Flush()

            content.Seek(0L, SeekOrigin.Begin) |> ignore
            serializeMessage context content

            do! endpoint.TriggerRequestReady(
                RequestReadyEventArgs(
                    httpRequestMessage,
                    header,
                    methodMap.ServiceCode,
                    methodMap.ServiceVersion |> Option.defaultValue ""
                )
            )
        }

type public XRoadUtil =
    static member MakeServiceCall<'T>(
        endpoint: AbstractEndpointDeclaration,
        methodName: string,
        header: XRoadHeader,
        args: obj[],
        mapValue: obj -> obj,
        mapResult: obj * BinaryContent seq -> 'T,
        cancellationToken: CancellationToken
    ) : Task<'T> =
        task {
            let serviceMethod = endpoint.GetType().GetMethod($"%s{methodName}Async", BindingFlags.Instance ||| BindingFlags.DeclaredOnly ||| BindingFlags.NonPublic ||| BindingFlags.Public)
            let serviceMethodMap = getMethodMap serviceMethod
            use httpRequestMessage = new HttpRequestMessage(HttpMethod.Post, "")
            let header = XRoadHeader(header)
            if String.IsNullOrWhiteSpace(header.Id) then
                header.Id <- getUUID()
            let xRoadHttpRequest = XRoadHttpRequest(endpoint, serviceMethodMap, header, httpRequestMessage)
            do! xRoadHttpRequest.SerializeMessage(args)
            use! httpResponseMessage = endpoint.HttpClient.SendAsync(httpRequestMessage, cancellationToken)
            httpResponseMessage.EnsureSuccessStatusCode() |> ignore
            let xRoadHttpResponse = XRoadHttpResponse(endpoint, serviceMethodMap, header, httpResponseMessage)
            let! responseValue = xRoadHttpResponse.DeserializeMessage()
            let mappedValue = mapValue responseValue
            let result = mapResult (mappedValue, xRoadHttpResponse.Attachments |> Seq.map _.Value)
            return result
        }
