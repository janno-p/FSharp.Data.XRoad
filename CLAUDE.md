# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Restore tools and dependencies (required first time or after paket.dependencies changes)
dotnet tool restore
dotnet paket restore

# Build (debug)
dotnet build

# Build (release)
dotnet build -c release

# Pack NuGet package
paket pack src/FSharp.Data.XRoad --version <version>
```

## Running Tests

```bash
# Run all tests
dotnet test

# Run a specific test project
dotnet test tests/FSharp.Data.XRoad.Tests/FSharp.Data.XRoad.Tests.fsproj
dotnet test tests/FSharp.Data.XRoad.DesignTime.Tests/FSharp.Data.XRoad.DesignTime.Tests.fsproj

# Run tests targeting a specific framework
dotnet test --framework net10.0
dotnet test --framework net472
```

The design-time tests optionally connect to a live X-Road security server. To enable this, create `tests/FSharp.Data.XRoad.DesignTime.Tests/appsettings.user.json`:
```json
{ "Host": "<security-server-url>", "Thumbprint": "<cert-thumbprint>" }
```

## Architecture

This is an F# type provider for X-Road (the Nordic/Baltic e-government data exchange layer). It follows the standard split-assembly type provider pattern from FSharp.TypeProviders.SDK.

### Assembly Split

**Runtime assembly** (`src/FSharp.Data.XRoad/`):
- Targets `netstandard2.0`, `netstandard2.1`, `net472`
- Contains types shipped to consumers: identifiers (`XRoadMemberIdentifier`, `XRoadServiceIdentifier`), `XRoadHeader`, `BinaryContent`, `AbstractEndpointDeclaration`, serialization delegates, and SOAP multipart handling
- Key shared files compiled into *both* runtime and design-time assemblies

**Design-time assembly** (`src/FSharp.Data.XRoad.DesignTime/`):
- Also targets `netstandard2.0`, `netstandard2.1`, `net472`
- Compiled first (as a `BeforeBuild` target inside the runtime project), then its output is copied into `typeproviders/fsharp41/` subdirectories alongside the runtime DLL
- Includes the ProvidedTypes SDK (`paket-files/fsprojects/FSharp.TypeProviders.SDK/src/ProvidedTypes.fs`)
- Recompiles the runtime source files directly (not by reference) to satisfy the type provider SDK requirement of no external runtime dependencies

### Design-time Pipeline (file order matters in F#)

| File | Responsibility |
|------|---------------|
| `FSharp.Data.XRoad.Xml.fs` | Low-level XML attribute/element helpers |
| `FSharp.Data.XRoad.Wsdl.fs` | WSDL document object model; active patterns for XSD/SOAP names; `SystemType` mapping (XSD → .NET/NodaTime types) |
| `FSharp.Data.XRoad.Http.fs` | HTTP fetching of WSDL from security server |
| `FSharp.Data.XRoad.Schema.fs` | XSD schema parsing into an intermediate representation |
| `FSharp.Data.XRoad.Builder.fs` | Converts parsed schema into `ProvidedTypeDefinition` trees using quotations |
| `FSharp.Data.XRoad.DesignTime.fs` | Two type providers: `XRoadInstanceProvider` (discovers services on a security server) and `XRoadProducerProvider` (generates typed client from a WSDL URL) |

### Runtime Source Files (also compiled into design-time)

| File | Responsibility |
|------|---------------|
| `FSharp.Data.XRoad.fs` | Public identifier types, `XRoadHeader`, `BinaryContent`, serializer delegate types, SOAP multipart reader |
| `FSharp.Data.XRoad.Attributes.fs` | Custom attributes for type provider–generated code (`XRoadTypeAttribute`, `XRoadElementAttribute`, etc.) |
| `FSharp.Data.XRoad.Choices.fs` | Choice type helpers (`Optional` wrapper integration) |
| `FSharp.Data.XRoad.Emit.fs` | IL emit helpers for serializer/deserializer generation at runtime |
| `FSharp.Data.XRoad.Protocol.fs` | SOAP envelope serialization/deserialization |
| `FSharp.Data.XRoad.MetaServices.fs` | X-Road meta-service calls (listMethods, allowedMethods, etc.) |
| `FSharp.Data.XRoad.Runtime.fs` | Runtime execution: sending requests, receiving responses |

### Key Design Decisions

- **NodaTime** is used for all date/time types (`OffsetDate`, `OffsetDateTime`, `OffsetTime`, `Period`) instead of BCL types, matching XSD semantics precisely.
- **Optional** library is used for choice type members.
- The design-time tests (`FSharp.Data.XRoad.DesignTime.Tests`) compile all source files directly (not via project reference) to replicate how the type provider SDK loads design-time logic, keeping tests isolated from runtime assembly loading.
- The runtime tests (`FSharp.Data.XRoad.Tests`) reference the runtime project normally.
