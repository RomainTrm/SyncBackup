module SyncBackup.Cli.Rules

open System
open Argu
open SyncBackup.Domain
open Microsoft.FSharp.Reflection

let solveConflict logger (rule1: Dsl.Rule) (rule2: Dsl.Rule) : Result<Dsl.Rule, string> =
    logger $"Rules conflict for path \"{rule1.Path.Path}\":"
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

type Rule =
    | Add of Rule: string * Path: string
    | List
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Add _ ->
                let availableRules =
                    FSharpType.GetUnionCases(typeof<Dsl.SyncRules>)
                    |> Seq.map (fun rule -> FSharpValue.MakeUnion(rule, [||]) :?> Dsl.SyncRules)
                    |> Seq.map Dsl.SyncRules.getValue
                    |> fun rules -> String.Join('|', rules)
                $"Add a new rule to the repository. Available rules: {availableRules}"
            | List -> "Display all rules."

let runCommand commandInfra queryInfra = function
    | Add (name, path) ->
        name
        |> Dsl.SyncRules.parse
        // TODO : build path correctly
        |> Result.map (fun rule -> ({ SyncRule = rule; Path = { Path = path; ContentType = Dsl.File; PathType = Dsl.Source } }: Dsl.Rule))
        |> Result.bind (SyncBackup.Commands.Config.Rules.add commandInfra)
        |> Result.map (fun () -> "Rule added")

    | List ->
        SyncBackup.Queries.Config.Rules.list queryInfra
        |> Result.map (fun rules -> String.Join(SyncBackup.Infra.Dsl.NewLine, rules))
