module FSharp.Data.XRoad.DesignTime.Common

open Microsoft.Extensions.Configuration

let configuration =
    (ConfigurationBuilder())
        .AddJsonFile("appsettings.json")
        .AddJsonFile("appsettings.user.json", true)
        .Build()

let host = configuration.["Host"]
