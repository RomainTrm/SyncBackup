module SyncBackup.Cli.Scan

open Argu

type Scan =
    | Run
    | Reset
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Run -> "Scan repository content and display every changes since last scan."
            | Reset -> "Reset scan state, next scan will display all content like a new repository."

let runCommand commandInfra = function
    | Run ->
        SyncBackup.Commands.Scan.scanRepositoryContent commandInfra ()
        |> Result.map (fun () -> "Scan completed.")
    | Reset ->
        SyncBackup.Commands.Scan.reset commandInfra ()
        |> Result.map (fun () -> "Repository reset.")

