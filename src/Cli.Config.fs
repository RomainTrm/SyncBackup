module SyncBackup.Cli.Config

open System
open Argu

module ConfigInit =
    type Init = | [<Hidden>] NoOption
    with interface IArgParserTemplate with member this.Usage = ""

    let runCommand commandInfra =
        SyncBackup.Commands.Config.Init.run commandInfra
        |> Result.map (fun () -> "Repository initialized")

module Aliases =
    type Alias =
        | Add of Name: string * Path: string
        | List
    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Add _ -> "Add a new alias to the repository."
                | List -> "Display all aliases."

    let runCommand commandInfra queryInfra = function
        | Add (name, path) ->
            ({ Name = name; Path = path }: SyncBackup.Domain.Dsl.Alias)
            |> SyncBackup.Commands.Config.Alias.add commandInfra
            |> Result.map (fun () -> "Alias added.")

        | List ->
            SyncBackup.Queries.Config.Alias.list queryInfra
            |> Result.map (fun aliases -> String.Join(Environment.NewLine, aliases))

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
    let currentDirectory = Environment.CurrentDirectory
    let commandInfra: SyncBackup.Commands.Config.Infra = {
        InitConfig = SyncBackup.Infra.Config.init currentDirectory
        LoadConfig = fun () -> SyncBackup.Infra.Config.load currentDirectory
        CheckPathExists = SyncBackup.Infra.Config.checkPathExists
        UpdateConfig = SyncBackup.Infra.Config.update currentDirectory
    }

    let queryInfra: SyncBackup.Queries.Config.Infra = {
        LoadConfig = fun () -> SyncBackup.Infra.Config.load currentDirectory
    }

    let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
    results.TryGetSubCommand()
    |> Option.bind (function
        | Init _ -> ConfigInit.runCommand commandInfra |> Some
        | Alias command ->
            command.GetAllResults ()
            |> List.tryExactlyOne
            |> Option.map (Aliases.runCommand commandInfra queryInfra)
    )
    |> Option.defaultWith (fun () -> Ok (parser.PrintUsage()))
    |> function
        | Ok value -> value
        | Error error -> $"An error occured: {error}"
