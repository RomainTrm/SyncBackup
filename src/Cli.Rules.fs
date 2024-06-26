﻿module SyncBackup.Cli.Rules

open System
open Argu
open Microsoft.FSharp.Core
open SyncBackup.Domain

let solveConflict logger (rule1: Dsl.Rule) (rule2: Dsl.Rule) : Result<Dsl.Rule, string> =
    logger $"Rules conflict for path \"{rule1.Path.Value}\":"
    logger $"1: {Dsl.SyncRules.getValue rule1.SyncRule}"
    logger $"2: {Dsl.SyncRules.getValue rule2.SyncRule}"

    let rec solveConflict' () =
        logger "Choose rule to keep (1/2/q):"
        match Console.ReadLine () with
        | "1" -> Ok rule1
        | "2" -> Ok rule2
        | "q" -> Error "Add rule aborted"
        | _ -> solveConflict' ()
    solveConflict' ()

let solveContentType logger : Result<Dsl.ContentType, string> =
    logger "Is the target a file or a directory?"
    logger "f: file"
    logger "d: directory"

    let rec solveContentType' () =
        logger "Choose type (f/d/q):"
        match Console.ReadLine () with
        | "f" -> Ok Dsl.File
        | "d" -> Ok Dsl.Directory
        | "q" -> Error "Add rule aborted"
        | _ -> solveContentType' ()
    solveContentType' ()

type Rule =
    | Add of Rule: string * Path: string
    | List
    | Edit
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Add _ ->
                let availableRules =
                    Dsl.SyncRules.getRulesAvailable
                    >> Seq.map Dsl.SyncRules.getDescription
                    >> Seq.map (sprintf "\t- %s")
                    >> fun rules -> String.Join(SyncBackup.Infra.Dsl.NewLine, rules)
                $"""Add a new rule to the repository.
Available rules (source repository):
{availableRules Dsl.RepositoryType.Source}
Available rules (backup repository):
{availableRules Dsl.RepositoryType.Backup}
Path: relative path into the repository."""
            | List -> "Display all rules."
            | Edit -> "Open a file with all tracked elements and associated rules for editions."

let runCommand commandInfra queryInfra = function
    | Add (name, path) ->
        name
        |> Dsl.SyncRules.parse
        |> Result.bind (fun rule -> SyncBackup.Commands.Config.Rules.add commandInfra rule path)
        |> Result.map (fun () -> "Rule added")

    | List ->
        SyncBackup.Queries.Config.Rules.list queryInfra
        |> Result.map (fun rules -> String.Join(SyncBackup.Infra.Dsl.NewLine, rules))

    | Edit ->
        SyncBackup.Commands.Config.Rules.editRules commandInfra
        |> Result.map (fun () -> "Rules saved")
