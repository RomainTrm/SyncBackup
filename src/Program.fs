open Argu
open SyncBackup.Cli.Config

let private succeedExit = 0

let logger = printfn "%s"

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<Commands>()
        runCommand parser argv
        |> logger
    with
        | e -> logger (e.ToString())

    succeedExit
