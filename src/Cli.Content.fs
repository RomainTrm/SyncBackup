module SyncBackup.Cli.Content

open Argu

type Content =
    | Scan
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Scan -> "Display all directories and files on repository."

let runCommand commandInfra = function
    | Scan ->
        SyncBackup.Commands.Content.scanRepositoryContent commandInfra ()
        |> Result.map (fun () -> "Scan completed.")
