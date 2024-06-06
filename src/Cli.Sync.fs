module SyncBackup.Cli.Sync

open System
open Argu

type Process =
    | BackupPath of string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | BackupPath _ -> "Absolute path to the root of the backup repository"


let private confirmProcess logger =
    logger "Warning: before processing synchronization, make sure your repository scans are up to date."
    logger "To run a scan, run: 'sync scan --run' on the repository."

    let rec confirmProcess' () =
        logger "Run synchronization? (y/n):"
        match Console.ReadLine () with
        | "y" -> true
        | "n" -> false
        | _ -> confirmProcess' ()
    confirmProcess' ()

let runCommand commandInfraFactory logger = function
    | BackupPath path ->
        match confirmProcess logger with
        | true ->
            SyncBackup.Domain.Dsl.DirectoryPath.build path
            |> commandInfraFactory
            |> SyncBackup.Commands.Sync.sync
        | false -> Error "Synchronization aborted!"
