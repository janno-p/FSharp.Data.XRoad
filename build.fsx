#r "paket: groupref fake //"

#if !FAKE
#load ".fake/build.fsx/intellisense.fsx"
#r "netstandard"
#endif

open System
open System.IO
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let (!!) includes = (!! includes).SetBaseDirectory __SOURCE_DIRECTORY__

let project = "FSharp.Data.XRoad"
let authors = "Janno Põldma"
let summary = "Type providers for generating types and service interfaces for XRoad producers"
let description = "Type providers for generating types and service interfaces for XRoad producers"
let tags = "F# fsharp typeproviders FSharp.Data.XRoad x-road xroad x-tee xtee"

let gitOwner = "janno-p"
let gitHome = "https://github.com/" + gitOwner
let gitName = "FSharp.Data.XRoad"

let packageProjectUrl = "https://janno-p.github.io/FSharp.Data.XRoad/"
let repositoryType = "git"
let repositoryUrl = "https://github.com/janno-p/FSharp.Data.XRoad"
let license = "MIT"

let release = ReleaseNotes.load "RELEASE_NOTES.md"

Target.create "AssemblyInfo" <| fun _ ->
    for file in !! "src/AssemblyInfo*.fs" do
        let replace (oldValue: string) newValue (str: string) = str.Replace(oldValue, newValue)
        let title =
            Path.GetFileNameWithoutExtension file
            |> replace "AssemblyInfo" "FSharp.Data.XRoad"
        let versionSuffix = ".0"
        let version = release.AssemblyVersion + versionSuffix
        AssemblyInfoFile.createFSharp file
            [ AssemblyInfo.Title title
              AssemblyInfo.Product project
              AssemblyInfo.Description summary
              AssemblyInfo.Version version
              AssemblyInfo.FileVersion version
            ]

Target.create "Clean" <| fun _ ->
    seq {
        yield! !!"**/bin"
        yield! !!"**/obj"
    } |> Shell.cleanDirs

Target.create "CleanDocs" <| fun _ ->
    Shell.cleanDirs ["docs/output"]

Target.create "Build" <| fun _ ->
    "FSharp.Data.XRoad.sln"
    |> DotNet.build (fun o ->
        { o with Configuration = DotNet.BuildConfiguration.Release })

Target.create "RunTests" <| fun _ ->
    "FSharp.Data.XRoad.sln"
    |> DotNet.test (fun o ->
        { o with Configuration = DotNet.BuildConfiguration.Release })

Target.create "NuGet" <| fun _ ->
    let releaseNotes = release.Notes |> String.concat "\n"

    let properties = [
        ("Version", release.NugetVersion)
        ("Authors", authors)
        ("PackageProjectUrl", packageProjectUrl)
        ("PackageTags", tags)
        ("RepositoryType", repositoryType)
        ("RepositoryUrl", repositoryUrl)
        ("PackageLicenseExpression", license)
        ("PackageReleaseNotes", releaseNotes)
        ("Summary", summary)
        ("PackageDescription", description)
        ("EnableSourceLink", "true")
        ("PublishRepositoryUrl", "true")
        ("EmbedUntrackedSources", "true")
        ("IncludeSymbols", "true")
        ("SymbolPackageFormat", "snupkg")
    ]

    DotNet.pack (fun p ->
        { p with
            Configuration = DotNet.BuildConfiguration.Release
            OutputPath = Some "bin"
            MSBuildParams = { p.MSBuildParams with Properties = properties }
        }
    ) "src/FSharp.Data.XRoad/FSharp.Data.XRoad.fsproj"

Target.create "GenerateDocs" <| fun _ ->
    Shell.cleanDir ".fsdocs"
    let args = String.Join (" ", [
        "build"
        "--properties"
        "Configuration=Release"
        "--strict"
        "--eval"
        "--clean"
        "--parameters"
        "fsdocs-package-version"
        release.NugetVersion
    ])
    DotNet.exec id "fsdocs" args |> ignore

Target.create "All" ignore

"Clean" ==> "AssemblyInfo" ==> "Build"
"Build" ==> "CleanDocs" ==> "GenerateDocs" ==> "All"
"Build" ==> "NuGet" ==> "All"
"Build" ==> "All"
"Build" ==> "RunTests" ==> "All"

Target.runOrList ()
