module SyncBackup.Cli.Rules

open Argu
open SyncBackup.Domain

let solveConflict logger (rule1: Dsl.Rule) (rule2: Dsl.Rule) : Result<Dsl.Rule, string> =
    Error "Not implemented"

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
