open Argu
open SyncBackup.Cli

let private succeedExit = 0

let logger = printfn "%s"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Worker.Commands>()
    try
        Worker.runCommand parser argv |> logger
    with
#if DEBUG
        | e -> logger (e.ToString())
#else
        | _ -> parser.PrintUsage() |> logger
#endif
    succeedExit
