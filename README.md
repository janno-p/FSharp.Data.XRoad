# FSharp.Data.XRoad

An F# type provider for [X-Road](https://x-road.global/) — the Nordic/Baltic e-government data exchange layer. Generates strongly-typed clients directly from a security server or a WSDL URL, with full support for SOAP/MIME multipart messaging.

## Features

- **`XRoadProducerProvider`** — generates a typed client from a WSDL URL or X-Road security server endpoint
- **`XRoadInstanceProvider`** — discovers available services on a security server at design time
- XSD ↔ .NET type mapping via [NodaTime](https://nodatime.org/) for precise date/time semantics
- SOAP multipart (MTOM-style binary attachment) support
- IL-emit based serializer/deserializer generation at runtime
- Targets `netstandard2.0`, `netstandard2.1`, and `net472`

## Quick Start

```fsharp
open FSharp.Data

// Generate a typed client from a WSDL URL
type Producer = XRoadProducerProvider<"https://security-server/wsdl?xRoadInstance=...&...">

let client = Producer.MyService.MyPort()
let response = client.MyMethod(request)
```

To discover available services on a security server:

```fsharp
type Instance = XRoadInstanceProvider<"https://security-server">

// Lists all producers available on the instance
Instance.Producers
```

## Installation

```
dotnet add package FSharp.Data.XRoad
```

Or with Paket:

```
nuget FSharp.Data.XRoad
```

## Building from Source

```bash
# Restore tools and dependencies
dotnet tool restore
dotnet paket restore

# Debug build
dotnet build

# Release build
dotnet build -c release

# Pack NuGet package
paket pack src/FSharp.Data.XRoad --version <version>
```

## Running Tests

```bash
# All tests
dotnet test

# Specific project
dotnet test tests/FSharp.Data.XRoad.Tests/FSharp.Data.XRoad.Tests.fsproj
dotnet test tests/FSharp.Data.XRoad.DesignTime.Tests/FSharp.Data.XRoad.DesignTime.Tests.fsproj

# Target a specific framework
dotnet test --framework net10.0
dotnet test --framework net472
```

The design-time tests can optionally connect to a live X-Road security server. Create `tests/FSharp.Data.XRoad.DesignTime.Tests/appsettings.user.json` to enable them:

```json
{ "Host": "<security-server-url>", "Thumbprint": "<cert-thumbprint>" }
```

## Architecture

The library follows the standard split-assembly type provider pattern from [FSharp.TypeProviders.SDK](https://github.com/fsprojects/FSharp.TypeProviders.SDK).

### Assembly Split

**Runtime** (`src/FSharp.Data.XRoad/`): ships to consumers — identifier types, `XRoadHeader`, `BinaryContent`, IL-emit serializers, SOAP multipart handling.

**Design-time** (`src/FSharp.Data.XRoad.DesignTime/`): loaded by F# tooling only — WSDL/XSD parsing pipeline, `ProvidedTypeDefinition` construction, HTTP fetching from security server.

### Design-time Pipeline

```
Http.fs → Wsdl.fs → Schema.fs → Builder.fs → DesignTime.fs
```

| File | Responsibility |
|------|----------------|
| `FSharp.Data.XRoad.Xml.fs` | Low-level XML helpers |
| `FSharp.Data.XRoad.Wsdl.fs` | WSDL DOM; XSD/SOAP active patterns; XSD → .NET type mapping |
| `FSharp.Data.XRoad.Http.fs` | WSDL fetching; MIME multipart parsing |
| `FSharp.Data.XRoad.Schema.fs` | XSD parsing into intermediate representation |
| `FSharp.Data.XRoad.Builder.fs` | Schema IR → `ProvidedTypeDefinition` trees |
| `FSharp.Data.XRoad.DesignTime.fs` | `XRoadInstanceProvider` + `XRoadProducerProvider` entry points |

### Key Design Decisions

- **NodaTime** for all date/time types (`OffsetDate`, `OffsetDateTime`, `OffsetTime`, `Period`) — matches XSD semantics precisely
- **Optional** library for XSD choice type members
- Runtime source files are recompiled into the design-time assembly (not referenced) to satisfy the type provider SDK requirement of no external runtime dependencies

## Dependencies

- [FSharp.Core](https://www.nuget.org/packages/FSharp.Core/) 6.0.7+
- [NodaTime](https://www.nuget.org/packages/NodaTime/)
- [Optional](https://www.nuget.org/packages/Optional/)

## License

[MIT](LICENSE.md)
