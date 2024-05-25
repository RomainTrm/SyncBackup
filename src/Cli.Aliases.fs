module SyncBackup.Cli.Aliases

open Argu
open System

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
