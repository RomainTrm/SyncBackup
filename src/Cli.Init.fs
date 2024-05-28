module SyncBackup.Cli.Init

open Argu

type Init = | Source
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Source -> "Initialize the current directory as a source directory to synchronize with backups."

let runCommand commandInfra = function
    | Source ->
        SyncBackup.Commands.Config.Init.source commandInfra
        |> Result.map (fun () -> "Repository initialized")
