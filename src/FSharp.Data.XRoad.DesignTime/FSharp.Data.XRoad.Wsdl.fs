module internal FSharp.Data.XRoad.Wsdl

open FSharp.Data.XRoad.Attributes
open System.Xml.Linq

[<AutoOpen>]
module internal Patterns =
    /// Matches names defined in `http://www.w3.org/2001/XMLSchema` namespace.
    let (|XsdName|_|) (name: XName) =
        match name.NamespaceName with
        | XmlNamespace.Xsd -> Some name.LocalName
        | _ -> None

    /// Matches elements defined in `http://www.w3.org/2001/XMLSchema` namespace.
    let (|Xsd|_|) (element: XElement) =
        match element.Name with
        | XsdName name -> Some name
        | _ -> None

    /// Matches names defined in `http://schemas.xmlsoap.org/soap/encoding/` namespace.
    let (|SoapEncName|_|) (name: XName) =
        match name.NamespaceName with
        | XmlNamespace.SoapEnc -> Some name.LocalName
        | _ -> None

    /// Matches type names which are mapped to system types.
    let (|SystemType|_|) = function
        | XsdName "anyURI" -> Some(typeof<string>, TypeHint.AnyUri)
        | XsdName "boolean" -> Some(typeof<bool>, TypeHint.Boolean)
        | XsdName "date" -> Some(typeof<NodaTime.OffsetDate>, TypeHint.Date)
        | XsdName "dateTime" -> Some(typeof<NodaTime.OffsetDateTime>, TypeHint.DateTime)
        | XsdName "decimal" -> Some(typeof<decimal>, TypeHint.Decimal)
        | XsdName "double" -> Some(typeof<double>, TypeHint.Double)
        | XsdName "duration" -> Some(typeof<NodaTime.Period>, TypeHint.Duration)
        | XsdName "float" -> Some(typeof<single>, TypeHint.Float)
        | XsdName "int" -> Some(typeof<int>, TypeHint.Int)
        | XsdName "integer" -> Some(typeof<bigint>, TypeHint.Integer)
        | XsdName "long" -> Some(typeof<int64>, TypeHint.Long)
        | XsdName "string" -> Some(typeof<string>, TypeHint.String)
        | XsdName "ID" -> Some(typeof<string>, TypeHint.Id)
        | XsdName "NMTOKEN" -> Some(typeof<string>, TypeHint.NmToken)
        | XsdName "token" -> Some(typeof<string>, TypeHint.Token)
        | XsdName name -> failwithf "Unmapped XSD type %s" name
        | SoapEncName name -> failwithf "Unmapped SOAP-ENC type %s" name
        | _ -> None

    let (|WsiName|_|) (name: XName) =
        match name.NamespaceName with
        | XmlNamespace.Wsi -> Some name.LocalName
        | _ -> None

    /// Matches system types which can be serialized as MIME multipart attachments:
    /// From X-Road service protocol: if the message is encoded as MIME container then values of all scalar elements
    /// of the input with type of either `xsd:base64Binary` or `xsd:hexBinary` will be sent as attachments.
    let (|BinaryType|_|) = function
        | XsdName "hexBinary"
        | XsdName "base64Binary"
        | SoapEncName "base64Binary" -> Some(TypeHint.None)
        | WsiName "swaRef" -> Some(TypeHint.SwaRef)
        | _ -> None

let isXRoadHeader (name: XName) =
    match name.NamespaceName, name.LocalName with
    | XmlNamespace.XRoad, ("client" | "service" | "centralService" | "id" | "userId" | "requestHash" | "issue" | "protocolVersion") -> true
    | _ -> false

/// Globally unique identifier for Xml Schema elements and types.
type SchemaName =
    | SchemaElement of XName
    | SchemaType of XName
    member this.XName
        with get() =
            match this with
            | SchemaElement(name)
            | SchemaType(name) -> name
    override this.ToString() =
        match this with
        | SchemaElement(name) -> sprintf "SchemaElement(%A)" name
        | SchemaType(name) -> sprintf "SchemaType(%A)" name

/// WSDL and SOAP binding style.
type BindingStyle =
    | Document
    | Rpc
    static member FromNode(node, ?defValue) =
        match node |> Xml.attr (XName.Get("style")) with
        | Some("document") -> Document
        | Some("rpc") -> Rpc
        | Some(v) -> failwithf "Unknown binding style value `%s`" v
        | None -> defaultArg defValue Document

/// Service method parameters for X-Road operations.
type Parameter =
    { Name: XName
      Type: XName option }

/// Combines parameter for request or response.
type OperationContent =
    { HasMultipartContent: bool
      Parameters: Parameter list
      RequiredHeaders: string list }

/// Type that represents different style of message formats.
type MethodCall =
    // Encoded always type
    | RpcEncoded of XName * OperationContent
    // Element directly under accessor element (named after message part name).
    // Type becomes the schema type of part accessor element.
    | RpcLiteral of XName * OperationContent
    // Encoded uses always type attribues.
    | DocEncoded of XNamespace * OperationContent
    // Element directly under body.
    // Type becomes the schema type of enclosing element (Body)
    | DocLiteral of OperationContent
    // Document literal with type part which defines SOAP:Body
    | DocLiteralBody of OperationContent
    // Recognizes valid document literal wrapped style operations.
    | DocLiteralWrapped of XName * OperationContent
    member this.IsEncoded =
        match this with RpcEncoded(_) | DocEncoded(_) -> true | _ -> false
    member this.Content =
        match this with
        | RpcEncoded(_,content) | RpcLiteral(_,content)
        | DocEncoded(_,content) | DocLiteral(content)
        | DocLiteralWrapped(_,content) | DocLiteralBody(content) -> content
    member this.RequiredHeaders =
        this.Content.RequiredHeaders
    member this.IsMultipart =
        this.Content.HasMultipartContent
    member this.Parameters =
        this.Content.Parameters

/// Definition for method which corresponds to single X-Road operation.
type ServicePortMethod =
    { Name: string
      Version: string option
      InputParameters: MethodCall
      OutputParameters: MethodCall
      Documentation: string option }

/// Collects multiple operations into logical group.
type ServicePort =
    { Name: string
      Documentation: string option
      Uri: string
      Methods: ServicePortMethod list }

/// All operations defined for single producer.
type Service =
    { Name: string
      Ports: ServicePort list
      Namespace: XNamespace }

/// Temporary type for SOAP:body binding elements.
type private SoapBody = {
    Parts: string list
    EncodingStyle: string option
    Namespace: string option
}

/// Temporary type for SOAP:header binding elements.
type private SoapHeader = {
    Message: XName
    Part: string
    EncodingStyle: string option
    Namespace: string option
}

/// Temporary type for MIME:content binding elements.
type private MimeContent = {
    Part: string
    Type: string option
}

/// Parse X-Road title elements for various languages.
let private readLanguages languageCode (element: XElement) =
    element.Elements(XName.Get("title", XmlNamespace.XRoad))
    |> Seq.fold (fun doc el ->
        let lang = el |> Xml.attrOrDefault (XName.Get("lang", XmlNamespace.Xml)) "et"
        (lang, el.Value)::doc) []
    |> List.tryFind (fst >> ((=) languageCode))
    |> Option.map snd

/// Read documentation element contents into language code indexed dictionary.
let private readDocumentation languageCode (element: XElement) =
    match element.Element(XName.Get("documentation", XmlNamespace.Wsdl)) with
    | null -> None
    | element -> readLanguages languageCode element

/// Parse qualified name for message attribute value.
let private parseMessageName name (element: XElement) =
    let messageElement = element.Element(XName.Get(name, XmlNamespace.Wsdl))
    messageElement
    |> Xml.reqAttr (XName.Get("message"))
    |> Xml.parseXName messageElement

/// Locate message element definition in WSDL document.
/// http://www.w3.org/TR/wsdl#_messages
let private findMessageElement definitions (name: XName) =
    // Default namespace for messages
    let targetNamespace = definitions |> Xml.attrOrDefault (XName.Get("targetNamespace")) ""
    if name.NamespaceName <> targetNamespace then
        failwithf "External messages are not supported yet! [%O]" name
    definitions.Elements(XName.Get("message", XmlNamespace.Wsdl))
    |> Seq.find (fun el -> (el |> Xml.reqAttr (XName.Get("name"))) = name.LocalName)

/// Collect parts of given message.
/// Returns name-indexed map of schema entities.
/// http://www.w3.org/TR/wsdl#_message
let private parseAbstractParts msgName (abstractDef: XElement) =
    abstractDef.Elements(XName.Get("part", XmlNamespace.Wsdl))
    |> Seq.map (fun elem ->
        let name = elem |> Xml.attrOrDefault (XName.Get("name")) ""
        match (elem |> Xml.attr (XName.Get("element"))), (elem |> Xml.attr (XName.Get("type"))) with
        | Some el, _ -> name, SchemaElement(Xml.parseXName elem el)
        | _, Some tp -> name, SchemaType(Xml.parseXName elem tp)
        | _ -> failwithf "Unknown element or type for message %s part %s" msgName name)
    |> Map.ofSeq

// Get encoding style used if any, and namespace for root element in RPC binding style.
let private getEncodingAndNamespace element =
    match element |> Xml.reqAttr (XName.Get("use")) with
    | "literal" -> None, None
    | "encoded" -> Some(element |> Xml.reqAttr (XName.Get("encodingStyle"))), element |> Xml.attr (XName.Get("namespace"))
    | encoding -> failwithf "Unexpected use attribute value `%s`" encoding

/// Parse primary operation parameters (operation body).
/// http://www.w3.org/TR/wsdl#_soap:body
let private parseSoapBody element =
    // Get encoding style used if any, and namespace for root element in RPC binding style.
    let encodingStyle, ns = getEncodingAndNamespace element
    // If parts attribute is defined, use space-separated list to get specified parts.
    // When no explicit parts are given, then all remaining parts not used in other blocks
    // are included into body.
    let parts =
        element
        |> Xml.attr (XName.Get("parts"))
        |> Option.map (fun value -> value.Split(' ') |> List.ofArray)
        |> Option.defaultValue []
    // Apply specified namespace to operation root (body) element.
    { Parts = parts; EncodingStyle = encodingStyle; Namespace = ns }

/// Parse header elements defined in concrete binding.
/// http://www.w3.org/TR/wsdl#_soap:header
let private parseSoapHeader element =
    // Get encoding style used if any, and namespace for root element in RPC binding style.
    let encodingStyle, ns = getEncodingAndNamespace element
    let messageName = element |> Xml.reqAttr (XName.Get("message")) |> Xml.parseXName element
    let partName = element |> Xml.reqAttr (XName.Get("part"))
    { Message = messageName; Part = partName; EncodingStyle = encodingStyle; Namespace = ns }

/// Get message parts from service operation binding.
let private parseBindingParts (binding: XElement) =
    // Validates SOAP:body contents.
    let foldBody oldBody elem isMultipart =
        oldBody |> Option.fold (fun newBody oldBody -> if isMultipart then newBody else oldBody) (parseSoapBody elem)
    // Parse binding elements.
    binding.Elements()
    |> Seq.fold (fun (bd: SoapBody option,hd,mp) elem ->
        match elem.Name.NamespaceName, elem.Name.LocalName with
        | XmlNamespace.Soap, "body" ->
            (Some(foldBody bd elem false),hd,mp)
        | XmlNamespace.Soap, "header" ->
            (bd, parseSoapHeader elem :: hd, mp)
        | XmlNamespace.Mime, "multipartRelated" ->
            elem.Elements(XName.Get("part", XmlNamespace.Mime))
            |> Seq.collect (fun elem -> elem.Elements())
            |> Seq.fold (fun (bd,hd,mp) elem ->
                match elem.Name.NamespaceName, elem.Name.LocalName with
                | XmlNamespace.Soap, "body" ->
                    (Some(foldBody bd elem true), hd, mp)
                | XmlNamespace.Soap, "header" ->
                    (bd, parseSoapHeader elem :: hd, mp)
                | XmlNamespace.Mime, "content" ->
                    let partName = elem |> Xml.reqAttr (XName.Get("part"))
                    let contentType = elem |> Xml.attr (XName.Get("type"))
                    (bd,hd, { Part = partName; Type = contentType } :: mp)
                | _ -> (bd,hd,mp)) (bd,hd,mp)
        | _ -> (bd,hd,mp)
        ) (None, [], [])

/// Partition all message parts into body and header components.
let private partitionMessageParts (abstractParts: Map<_,_>)  bodyPart contentParts headerParts messageName definitions abstractDef =
    let contentParts =
        contentParts
        |> List.map (fun part ->
            match abstractParts.TryFind part.Part with
            | Some(_) -> part.Part
            | None -> failwithf "Message `%s` does not contain part `%s`." messageName part.Part)
    let parts =
        headerParts
        |> List.map (fun part ->
            let message = findMessageElement definitions part.Message
            if message = abstractDef then Choice1Of3(part.Part)
            else
                let parts = message |> parseAbstractParts part.Message.LocalName
                match parts.TryFind part.Part with
                | Some(value) when isXRoadHeader value.XName -> Choice2Of3(part.Part)
                | Some(_) -> Choice3Of3(part.Part)
                | None -> failwithf "Message %s does not contain part %s" part.Message.LocalName part.Part)
    let hdr = parts |> List.choose (fun x -> match x with Choice1Of3(x) -> Some(x) | _ -> None)
    let reqHdr = parts |> List.choose (fun x -> match x with Choice2Of3(x) -> Some(x) | _ -> None)
    let excludedParts = List.concat [ contentParts; hdr ]
    let body = abstractParts |> Map.toList |> List.map (fst) |> List.filter (fun x -> not (excludedParts |> List.exists ((=) x)))
    if not (List.isEmpty bodyPart.Parts) then
        let count = bodyPart.Parts |> List.filter (fun x -> body |> List.exists((=) x)) |> List.length
        if count <> body.Length then failwithf "Not all message `%s` parts have corresponding bindings." messageName
    body, reqHdr

/// Check if literal part of message is correct.
let private validateLiteralParameters (parameters: Parameter list) messageName =
    let typeCount = parameters |> List.choose (fun x -> x.Type) |> List.length
    if typeCount > 1 || (typeCount = 1 && parameters.Length > 1)
    then failwithf "Literal operation message `%s` should have at most exactly one type reference in part definitions." messageName
      
/// Check if encoded part of message is correct.
let private validateEncodedParameters (parameters: Parameter list) messageName =
    if parameters |> List.exists (fun x -> x.Type.IsNone)
    then failwithf "Encoded operation message `%s` should not have element references in part definitions." messageName

/// Read operation message and its parts definitions from document.
/// http://www.w3.org/TR/wsdl#_abstract-v
let private parseOperationMessage style (binding: XElement) definitions abstractDef opName ns =
    let msgName = abstractDef |> Xml.reqAttr (XName.Get("name"))
    let abstractParts = abstractDef |> parseAbstractParts msgName
    // Walk through message parts explicitly referenced in operation binding.
    let bodyPart, headerParts, contentParts = parseBindingParts binding
    // Take wrapper name from body definition.
    let accessorName =
        match bodyPart with
        | Some(part) ->
            part.EncodingStyle
            |> Option.map (fun enc ->
                match enc with
                | XmlNamespace.SoapEnc -> XName.Get(opName, (part.Namespace |> Option.defaultValue ns))
                | _ -> failwithf "Unknown encoding style `%s` for `%s` operation SOAP:body." enc msgName)
        | None -> failwithf "X-Road operation binding `%s` doesn't define SOAP:body." msgName
    // Build service parameters.
    let expectedBodyParts, requiredHeaders =
        partitionMessageParts abstractParts bodyPart.Value contentParts headerParts msgName definitions abstractDef
    let parameters =
        expectedBodyParts
        |> List.map (fun partName ->
            match abstractParts.TryFind partName with
            | Some(SchemaElement(name)) -> { Name = name; Type = None }
            | Some(SchemaType(name)) -> { Name = XName.Get(partName); Type = Some(name) }
            | None -> failwithf "Message `%s` does not contain part `%s`." msgName partName)
    // Body parts should be described uniformly.
    let numTypes = parameters |> List.filter (fun x -> x.Type.IsNone) |> List.length
    let numElements = parameters |> List.filter (fun x -> x.Type.IsSome) |> List.length
    if numTypes > 0 && numElements > 0 then
        failwithf "Mixing type and element parts in operation message (%s) is not acceptable." opName
    // Service request input or output parameters.
    let content = {
        HasMultipartContent = contentParts |> List.isEmpty |> not
        Parameters = parameters
        RequiredHeaders = requiredHeaders
    }
    // Validate parameter usage
    match accessorName with
    | Some(_) -> validateEncodedParameters content.Parameters msgName
    | None -> validateLiteralParameters content.Parameters msgName
    // Wrap method call into correct context.
    match style, accessorName with
    | Document, Some(value) -> DocEncoded(value.Namespace, content)
    | Document, None ->
        match content with
        | { Parameters = [ { Type = Some(_) } ] } -> DocLiteralBody(content)
        | { Parameters = { Type = Some(_) } :: _ } ->
            failwithf "Document literal style can have exactly 1 type part in operation message (%s)." opName
        | { Parameters = [ { Name = name; Type = None } ] } -> DocLiteralWrapped(name, content)
        | _ -> DocLiteral(content)
    | Rpc, Some(value) -> RpcEncoded(value, content)
    | Rpc, None -> RpcLiteral(XName.Get(opName, ns), content)

/// Parse operation binding and bind to abstract message definitions.
/// http://www.w3.org/TR/wsdl#_bindings
let private parseOperation languageCode filter operation (portType: XElement) definitions style ns =
    let name = operation |> Xml.reqAttr (XName.Get("name"))
    if not (filter |> List.isEmpty || filter |> List.exists ((=) name)) then None else
    // Extract X-Road version of the operation (optional: not used for metaservice operations).
    let version =
        match operation.Element(XName.Get("version", XmlNamespace.XRoad)) with
        | null -> None
        | el -> Some el.Value
    // SOAP extension for operation element: http://www.w3.org/TR/wsdl#_soap:operation
    let style =
        match operation.Element(XName.Get("operation", XmlNamespace.Soap)) with
        | null -> style
        | soapOperation -> BindingStyle.FromNode(soapOperation, style)
    // Find abstract definition for the operation in matching portType element.
    let abstractDesc =
        let abstractOperation =
            portType.Elements(XName.Get("operation", XmlNamespace.Wsdl))
            |> Seq.tryFind (fun op -> (op |> Xml.reqAttr (XName.Get("name"))) = name)
        match abstractOperation with
        | Some(op) -> op
        | None -> failwithf "Unable to find abstract definition for operation `%s` binding." name
    // Parse parameters for message input or output parameters.
    let parseParameters direction =
        let message = abstractDesc |> parseMessageName direction |> findMessageElement definitions
        let bindingElement = (operation.Element(XName.Get(direction, XmlNamespace.Wsdl)))
        parseOperationMessage style bindingElement definitions message name ns
    // Combine abstract and concrete part information about service implementation.
    Some({
        Name = name
        Version = version
        InputParameters = parseParameters "input"
        OutputParameters = parseParameters "output"
        Documentation = readDocumentation languageCode abstractDesc
    })

/// Parse operations bindings block.
/// http://www.w3.org/TR/wsdl#_bindings
let private parseBinding languageCode operationFilter definitions (bindingName: XName) (servicePort: ServicePort) =
    // Default namespace for operations
    let targetNamespace = definitions |> Xml.attrOrDefault (XName.Get("targetNamespace")) ""
    // Find binding element in current document
    if bindingName.NamespaceName <> targetNamespace then
        failwithf "External namespaces are not yet supported! Given %s." bindingName.NamespaceName
    let binding =
        definitions.Elements(XName.Get("binding", XmlNamespace.Wsdl))
        |> Seq.find (fun el -> (el |> Xml.reqAttr (XName.Get("name"))) = bindingName.LocalName)
    // Find portType element in current document for abstract part definitions.
    let portTypeName = binding |> Xml.reqAttr (XName.Get("type")) |> Xml.parseXName binding
    let ns = portTypeName.NamespaceName
    if ns <> targetNamespace then
        failwithf "External namespaces are not yet supported! Given %s." portTypeName.NamespaceName
    let portType =
        definitions.Elements(XName.Get("portType", XmlNamespace.Wsdl))
        |> Seq.find (fun el -> (el |> Xml.reqAttr (XName.Get("name"))) = portTypeName.LocalName)
    // SOAP extension for binding element: http://www.w3.org/TR/wsdl#_soap:binding
    let soapBinding = binding.Element(XName.Get("binding", XmlNamespace.Soap))
    let bindingStyle = BindingStyle.FromNode(soapBinding)
    // X-Road specification allows only HTTP transport.
    let transport = soapBinding |> Xml.attrOrDefault (XName.Get("transport")) ""
    if transport <> XmlNamespace.Http then
        failwithf "Only HTTP transport is allowed. Specified %s" transport
    // Parse individual operations from current binding element.
    let methods =
        binding.Elements(XName.Get("operation", XmlNamespace.Wsdl))
        |> Seq.choose (fun op -> parseOperation languageCode operationFilter op portType definitions bindingStyle ns)
        |> List.ofSeq
    { servicePort with Methods = methods }

/// Parse port binding element contents.
/// http://www.w3.org/TR/wsdl#_ports
let private parsePortBinding languageCode operationFilter definitions element =
    let name = element |> Xml.reqAttr (XName.Get("name"))
    let binding = element |> Xml.reqAttr (XName.Get("binding")) |> Xml.parseXName element
    // http://www.w3.org/TR/wsdl#_soap:address
    let address =
        match element.Element(XName.Get("address", XmlNamespace.Soap)) with
        | null -> ""
        | e -> e |> Xml.reqAttr (XName.Get("location"))
    // Build port binding object if available.
    let servicePort = {
        Name = name
        Documentation = readLanguages languageCode element
        Uri = address
        Methods = []
    }
    Some(servicePort |> parseBinding languageCode operationFilter definitions binding)

/// Parse all service elements defined as immediate child elements of current element.
/// http://www.w3.org/TR/wsdl#_services
let parseServices languageCode operationFilter (definitions: XElement) =
    let targetNamespace = XNamespace.Get(definitions.Attribute(XName.Get("targetNamespace")).Value)
    definitions.Elements(XName.Get("service", XmlNamespace.Wsdl))
    |> Seq.map (fun service ->
        let ports =
            service.Elements(XName.Get("port", XmlNamespace.Wsdl))
            |> Seq.choose (parsePortBinding languageCode operationFilter definitions)
            |> List.ofSeq
        { Name = service |> Xml.reqAttr (XName.Get("name")); Ports = ports; Namespace = targetNamespace })
    |> List.ofSeq
