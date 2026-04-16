module FSharp.Data.XRoad.DesignTime.Common

open System
open System.Net
open System.Security.Cryptography
open Microsoft.Extensions.Configuration

let configuration =
    ConfigurationBuilder()
        .AddJsonFile("appsettings.json")
        .AddJsonFile("appsettings.user.json", true)
        .Build()

let host = configuration["Host"]
let thumbprint = configuration["Thumbprint"]

ServicePointManager.ServerCertificateValidationCallback <-
    fun sender cert chain errors ->
        cert.GetCertHashString().Equals(thumbprint, StringComparison.OrdinalIgnoreCase);
