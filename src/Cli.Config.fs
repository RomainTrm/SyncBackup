module SyncBackup.Cli.Config

open System
open Argu

type Commands =
    | [<CliPrefix(CliPrefix.None); AltCommandLine("init")>] Init_Repository of ParseResults<NoOption>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Init_Repository _ -> "Init current directory as directory to sync for backups"

and NoOption =
    | [<Hidden>] NoOption
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | NoOption -> "NoOption"

let runCommand (parser: ArgumentParser<Commands>) argv =
    let commandInfra: SyncBackup.Commands.Config.Infra = {
        InitConfig = SyncBackup.Infra.Config.init
        LoadConfig = fun _ -> failwith "not implemented"
        CheckPathExists = SyncBackup.Infra.Config.checkPathExists
        UpdateConfig = fun _ -> failwith "not implemented"
    }

    let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
    match results.GetSubCommand() with
    | Init_Repository _ ->
        let currentDirectory = Environment.CurrentDirectory
        SyncBackup.Commands.Config.Init.run commandInfra currentDirectory
        |> function
            | Ok () -> "Repository initialized"
            | Error error -> $"An error occured: {error}"
