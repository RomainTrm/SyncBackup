module SyncBackup.Cli.Worker

open System
open Argu

let private executeCommand<'c when 'c :> IArgParserTemplate> run (command: ParseResults<'c>) =
    command.GetAllResults ()
    |> List.tryExactlyOne
    |> Option.map run

type Commands =
    | [<CliPrefix(CliPrefix.None)>] Init of ParseResults<Init.Init>
    | [<CliPrefix(CliPrefix.None)>] Alias of ParseResults<Aliases.Alias>
    | [<CliPrefix(CliPrefix.None)>] Rules of ParseResults<Rules.Rule>
    | [<CliPrefix(CliPrefix.None)>] Scan of ParseResults<Scan.Scan>
    | [<CliPrefix(CliPrefix.None)>] Process of ParseResults<Sync.Process>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Init _ -> "Initialize the current directory as a repository to synchronize."
            | Alias _ -> "Manage aliases (pointers to directories outside the repository's directory), only available for the source repository."
            | Rules _ -> "Manage rules for synchronization."
            | Scan _ -> "Reference all directories and files in the repository."
            | Process _ -> "Run synchronization process between two repositories."

let runCommand (parser: ArgumentParser<Commands>) (logger: string -> unit) argv =
    let currentDirectory = Environment.CurrentDirectory
    let configCommandInfra = Factory.configCommandInfra logger currentDirectory
    let configQueryInfra = Factory.configQueryInfra currentDirectory
    let contentCommandInfra = Factory.contentCommandInfra currentDirectory
    let syncCommandInfraFactory = Factory.syncCommandInfra logger currentDirectory

    let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
    results.TryGetSubCommand()
    |> Option.bind (function
        | Init command -> command |> executeCommand (Init.runCommand configCommandInfra)
        | Alias command -> command |> executeCommand (Aliases.runCommand configCommandInfra configQueryInfra)
        | Rules command -> command |> executeCommand (Rules.runCommand configCommandInfra configQueryInfra)
        | Scan _ -> Scan.runCommand contentCommandInfra |> Some
        | Process command -> command |> executeCommand (Sync.runCommand syncCommandInfraFactory)
    )
    |> Option.defaultWith (fun () -> Ok (parser.PrintUsage()))
    |> function
        | Ok value -> logger value
        | Error error -> logger $"An error occured: {error}"
