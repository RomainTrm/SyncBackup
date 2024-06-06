open System
open System.IO
open Argu
open SyncBackup.Cli

let private succeedExit = 0

let logger = printfn "%s"

let logError e =
    let currentDirectory = Environment.CurrentDirectory
    let logFilePath = SyncBackup.Infra.Dsl.getErrorLogFilePath currentDirectory DateTime.Now
    File.WriteAllText (logFilePath, e.ToString())

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Worker.Commands>()
    try
        Worker.runCommand parser logger argv
    with
#if DEBUG
        | e ->
            logError e
            logger (e.ToString())
#else
        | e ->
            logError e
            parser.PrintUsage() |> logger
#endif
    succeedExit
