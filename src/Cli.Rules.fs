module SyncBackup.Cli.Rules

open System
open Argu
open SyncBackup.Domain

let solveConflict logger (rule1: Dsl.Rule) (rule2: Dsl.Rule) : Result<Dsl.Rule, string> =
    let path =
        match rule1.Path with
        | Dsl.Alias path -> path
        | Dsl.Source path -> path

    logger $"Rules conflict for path \"{path}\":"
    logger $"1 {Dsl.SyncRules.getValue rule1.SyncRule}"
    logger $"2 {Dsl.SyncRules.getValue rule2.SyncRule}"

    let rec solveConflict' () =
        logger "Choose rule to keep (1/2/q):"
        match Console.ReadLine () with
        | "1" -> Ok rule1
        | "2" -> Ok rule2
        | "q" -> Error "Add rule aborted"
        | _ -> solveConflict' ()
    solveConflict' ()

type Alias =
    | Add of Rule: string * Path: string

with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Add _ -> "Add a new rule to the repository."

let runCommand commandInfra queryInfra = function
    | Add (name, path) ->
        "not implemented"
