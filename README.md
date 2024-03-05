
This is a simple F# type provider.  It has separate design-time and runtime assemblies.

Paket is used to acquire the type provider SDK and build the nuget package (you can remove this use of paket if you like)

Building:

    dotnet tool restore

    dotnet paket restore

    dotnet build -c release

    paket pack src\FSharp.Data.XRoad --version 0.0.1

Status: https://github.com/fsprojects/FSharp.TypeProviders.SDK/commit/6149ed507f15caa68a475808bcf5b3c2f52a4d34
