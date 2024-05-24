module SyncBackup.Cli

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
            ({ Name = name; Path = SyncBackup.Domain.Dsl.DirectoryPath.build path }: SyncBackup.Domain.Dsl.Alias)
            |> SyncBackup.Commands.Config.Alias.add commandInfra
            |> Result.map (fun () -> "Alias added.")

        | List ->
            SyncBackup.Queries.Config.Alias.list queryInfra
            |> Result.map (fun aliases -> String.Join(SyncBackup.Infra.Dsl.NewLine, aliases))

module Content =
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
            |> Result.map (fun aliases -> String.Join(SyncBackup.Infra.Dsl.NewLine, aliases))

type Commands =
    | [<CliPrefix(CliPrefix.None)>] Init of ParseResults<ConfigInit.Init>
    | [<CliPrefix(CliPrefix.None)>] Alias of ParseResults<Aliases.Alias>
    | [<CliPrefix(CliPrefix.None)>] Content of ParseResults<Content.Content>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Init _ -> "Init the current directory as a source directory to sync with backups."
            | Alias _ -> "Manage aliases (pointers to directories outside the repository's directory), only available for the source directory."
            | Content _ -> "Manage content directories and files inside the repository."

let runCommand (parser: ArgumentParser<Commands>) argv =
    let currentDirectory = Environment.CurrentDirectory
    let configCommandInfra: SyncBackup.Commands.Config.Infra = {
        InitConfig = SyncBackup.Infra.Config.init currentDirectory
        LoadConfig = fun () -> SyncBackup.Infra.Config.load currentDirectory
        CheckPathExists = SyncBackup.Infra.Config.checkPathExists
        UpdateConfig = SyncBackup.Infra.Config.update currentDirectory
    }

    let configQueryInfra: SyncBackup.Queries.Config.Infra = {
        LoadConfig = fun () -> SyncBackup.Infra.Config.load currentDirectory
    }

    let contentCommandInfra: SyncBackup.Commands.Content.Infra = {
        LoadFiles = SyncBackup.Infra.Content.Scan.run currentDirectory
        LoadAliases = fun () -> SyncBackup.Infra.Config.load currentDirectory |> Result.map _.Aliases
        SaveTempContent = SyncBackup.Infra.Content.ScanFile.writeFile currentDirectory
        OpenForUserEdition = fun () ->
            SyncBackup.Infra.Dsl.getScanFileFilePath currentDirectory
            |> SyncBackup.Infra.Editor.VsCode.runEditor
        ReadTempContent = SyncBackup.Infra.Content.ScanFile.readFile currentDirectory
        SaveTrackFile = SyncBackup.Infra.Content.TrackFile.save currentDirectory
    }

    let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
    results.TryGetSubCommand()
    |> Option.bind (function
        | Init _ -> ConfigInit.runCommand configCommandInfra |> Some
        | Alias command ->
            command.GetAllResults ()
            |> List.tryExactlyOne
            |> Option.map (Aliases.runCommand configCommandInfra configQueryInfra)
        | Content command ->
            command.GetAllResults ()
            |> List.tryExactlyOne
            |> Option.map (Content.runCommand contentCommandInfra)
    )
    |> Option.defaultWith (fun () -> Ok (parser.PrintUsage()))
    |> function
        | Ok value -> value
        | Error error -> $"An error occured: {error}"
