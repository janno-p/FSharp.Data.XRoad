[<RequireQualifiedAccess>]
module internal FSharp.Data.XRoad.Xml

open System.Xml.Linq

/// Extracts optional attribute value from current element.
/// Returns None if attribute is missing.
let attr (name: XName) (element: XElement) =
    match element.Attribute(name) with
    | null -> None
    | attr -> Some attr.Value

/// Extracts optional attribute value from current element.
/// Return default value if attribute is missing.
let attrOrDefault name value element =
    element |> attr name |> Option.defaultValue value

/// Check if given node is constrained to use qualified form.
/// Returns true if node requires qualified name.
let isQualified attrName node =
    match node |> attrOrDefault attrName "unqualified" with
    | "qualified" -> true
    | "unqualified" -> false
    | x -> failwith $"Unknown %s{attrName.LocalName} value '%s{x}'"

/// Extracts value of required attribute from current element.
/// When attribute is not found, exception is thrown.
let reqAttr (name: XName) (element: XElement) =
    match element.Attribute name with
    | null -> failwith $"Element %A{element.Name} attribute %A{name} is required!"
    | attr -> attr.Value

/// Parse qualified name from given string.
let parseXName (element: XElement) (qualifiedName: string) =
    match qualifiedName.Split(':') with
    | [| name |] -> XName.Get(name, element.GetDefaultNamespace().NamespaceName)
    | [| prefix; name |] -> XName.Get(name, element.GetNamespaceOfPrefix(prefix).NamespaceName)
    | _ -> failwith $"Invalid qualified name string %s{qualifiedName}"
