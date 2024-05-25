module SyncBackup.Cli.Worker

open System
open Argu

let private executeCommand<'c when 'c :> IArgParserTemplate> run (command: ParseResults<'c>) =
    command.GetAllResults ()
    |> List.tryExactlyOne
    |> Option.map run

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

let runCommand (parser: ArgumentParser<Commands>) (logger: string -> unit) argv =
    let currentDirectory = Environment.CurrentDirectory
    let configCommandInfra = Factory.configCommandInfra logger currentDirectory
    let configQueryInfra = Factory.configQueryInfra currentDirectory
    let contentCommandInfra = Factory.contentCommandInfra currentDirectory

    let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
    results.TryGetSubCommand()
    |> Option.bind (function
        | Init _ -> ConfigInit.runCommand configCommandInfra |> Some
        | Alias command -> command |> executeCommand (Aliases.runCommand configCommandInfra configQueryInfra)
        | Content command -> command |> executeCommand (Content.runCommand contentCommandInfra)
    )
    |> Option.defaultWith (fun () -> Ok (parser.PrintUsage()))
    |> function
        | Ok value -> logger value
        | Error error -> logger $"An error occured: {error}"
