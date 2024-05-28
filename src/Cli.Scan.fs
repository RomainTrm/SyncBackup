module SyncBackup.Cli.Scan

open Argu

type Scan = | [<Hidden>] Scan
with interface IArgParserTemplate with member this.Usage = ""

let runCommand commandInfra =
    SyncBackup.Commands.Scan.scanRepositoryContent commandInfra ()
    |> Result.map (fun () -> "Scan completed.")
