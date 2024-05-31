module SyncBackup.Cli.Sync

open Argu

type Sync =
    | BackupPath of string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | BackupPath _ -> "Absolute path to the root of the backup repository"

let runCommand commandInfraFactory = function
    | BackupPath path ->
        commandInfraFactory path
        |> SyncBackup.Commands.Sync.sync
