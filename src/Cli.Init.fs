module SyncBackup.Cli.Init

open Argu

type Init =
    | Source
    | Backup
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Source -> "Initialize the current directory as a source repository to synchronize with backups."
            | Backup -> "Initialize the current directory as a backup repository."

let runCommand commandInfra = function
    | Source ->
        SyncBackup.Commands.Config.Init.source commandInfra
        |> Result.map (fun () -> "Repository initialized")
    | Backup ->
        SyncBackup.Commands.Config.Init.backup commandInfra
        |> Result.map (fun () -> "Repository initialized")
