namespace FSharp.Data.XRoad

open FSharp.Data.XRoad.Schema
open FSharp.Data.XRoad.Wsdl
open System
open System.Xml.Linq

/// Combines operations and types documented in producer definitions.
type internal ProducerDescription =
    { LanguageCode: string
      TypeSchemas: Map<string, SchemaNode>
      Services: Service list }

    /// Load producer definition from given uri location.
    static member Load(uri: Uri, languageCode, operationFilter) =
        let document = Http.getXDocument uri
        match document.Element(XName.Get("definitions", XmlNamespace.Wsdl)) with
        | null -> failwithf "Uri `%A` refers to invalid WSDL document (`definitions` element not found)." uri
        | definitions ->
            { LanguageCode = languageCode
              Services = definitions |> parseServices languageCode operationFilter
              TypeSchemas = definitions |> Parser.parseSchema (uri.ToString()) }

    /// Load producer definition from given XML document.
    static member Load(document: XDocument, languageCode, operationFilter) =
        match document.Element(XName.Get("definitions", XmlNamespace.Wsdl)) with
        | null -> failwith "Invalid WSDL document (`definitions` element not found)."
        | definitions ->
            let uri = definitions.Attribute(XName.Get("targetNamespace")).Value
            { LanguageCode = languageCode
              Services = definitions |> parseServices languageCode operationFilter
              TypeSchemas = definitions |> Parser.parseSchema uri }
