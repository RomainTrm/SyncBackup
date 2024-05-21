module SyncBackup.Cli.Config

open System
open Argu

module ConfigInit =
    type Init =
        | [<Hidden>] NoOption
    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | NoOption -> "NoOption"

    let runCommand commandInfra currentDirectory =
        SyncBackup.Commands.Config.Init.run commandInfra currentDirectory
        |> function
            | Ok () -> "Repository initialized"
            | Error error -> $"An error occured: {error}"

module Aliases =
    type Alias =
        | Add of ParseResults<AddAlias>
    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Add _ -> "Add a new alias to the repository."
    and AddAlias =
        | [<Mandatory>] Name of string
        | [<Mandatory>] Path of string
    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Name _ -> "The name of the alias, it will be used as the directory's name on the backup."
                | Path _ -> "The path to the directory to backup."

    let runCommand commandInfra currentDirectory (command: ParseResults<Alias>) =
        match command.GetSubCommand() with
        | Add options ->
            let name = options.GetResult <@ AddAlias.Name @>
            let path = options.GetResult <@ AddAlias.Path @>
            ({ Name = name; Path = path }: SyncBackup.Domain.Dsl.Alias)
            |> SyncBackup.Commands.Config.Alias.add commandInfra currentDirectory
            |> function
                | Ok () -> "Alias added."
                | Error error -> $"An error occured: {error}"

type Commands =
    | [<CliPrefix(CliPrefix.None)>] Init of ParseResults<ConfigInit.Init>
    | [<CliPrefix(CliPrefix.None)>] Alias of ParseResults<Aliases.Alias>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Init _ -> "Init the current directory as a source directory to sync with backups."
            | Alias _ -> "Manage aliases (pointers to directories outside the repository's directory), only available for the source directory."

let runCommand (parser: ArgumentParser<Commands>) argv =
    let commandInfra: SyncBackup.Commands.Config.Infra = {
        InitConfig = SyncBackup.Infra.Config.init
        LoadConfig = SyncBackup.Infra.Config.load
        CheckPathExists = SyncBackup.Infra.Config.checkPathExists
        UpdateConfig = SyncBackup.Infra.Config.update
    }

    let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
    let currentDirectory = Environment.CurrentDirectory

    match results.GetSubCommand() with
    | Init _ -> ConfigInit.runCommand commandInfra currentDirectory
    | Alias command -> Aliases.runCommand commandInfra currentDirectory command
