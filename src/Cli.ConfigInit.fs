module SyncBackup.Cli.ConfigInit

open Argu

type Init = | Source
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Source -> "Initialize the current directory as a source directory to synchronize with backups."

let runCommand commandInfra = function
    | Source ->
        SyncBackup.Commands.Config.Init.run commandInfra
        |> Result.map (fun () -> "Repository initialized")
