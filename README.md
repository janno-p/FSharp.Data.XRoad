
This is a simple F# type provider.  It has separate design-time and runtime assemblies.

Paket is used to acquire the type provider SDK and build the nuget package (you can remove this use of paket if you like)

Building:

    dotnet tool install paket -g

    paket update

    dotnet build -c release

    paket pack src\FSharp.Data.XRoad.Runtime --version 0.0.1
