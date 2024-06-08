module SyncBackup.Domain.Rules

open SyncBackup
open SyncBackup.Domain.Dsl

type Rule = Dsl.Rule
type SyncRules = Dsl.SyncRules

type AddRuleResult =
    | Added of Rule list
    | Conflict of Rule * Rule
    | RuleAlreadyThere

let add (rules: Rule list) = function
    | rule when rules |> List.contains rule -> RuleAlreadyThere
    | rule when rules |> List.exists (fun r -> r.Path = rule.Path) ->
        let existingRule = rules |> List.find (fun r -> r.Path = rule.Path)
        Conflict (existingRule, rule)
    | { SyncRule = NoRule } -> RuleAlreadyThere
    | rule -> Added (rules@[rule])

let replace (rules: Rule list) (rule: Rule) =
    let rules = rules |> List.filter (fun r -> r.Path <> rule.Path)
    match rule.SyncRule with
    | NoRule -> rules
    | _ -> rules |> List.append [rule]

let private buildRule (existingRules: Map<RelativePath, Rule>) (path: RelativePath) =
    existingRules
    |> Map.tryFind path
    |> Option.defaultValue { Path = path; SyncRule = NoRule }

let buildRulesForScanning (existingRules: Rule list) (paths: RelativePath list) =
    let existingRules = existingRules |> Seq.map (fun rule -> rule.Path, rule) |> Map
    paths |> List.map (buildRule existingRules)

let validateRule repositoryType rule =
    match repositoryType, rule with
    | RepositoryType.Source, SyncRules.AlwaysReplace
    | RepositoryType.Source, SyncRules.NotDelete
    | RepositoryType.Source, SyncRules.NotSave
    | RepositoryType.Backup, SyncRules.Include
    | RepositoryType.Backup, SyncRules.Exclude
        -> Error $"The rule \"{SyncRules.getValue rule}\" can't be applied to this repository type."
    | _ -> Ok ()

let buildTreeWithRules (existingRules: Rule list) (trackedElements: RelativePath list) =
    trackedElements
    |> buildRulesForScanning existingRules
    |> List.sortBy _.Path.Value
    |> Ok

let updateRulesAfterEdition oldRules =
    List.fold (fun rules rule ->
        match add rules rule with
        | Added rules -> rules
        | RuleAlreadyThere -> rules
        | Conflict _ ->
            // users chose to override value while editing the file, so we can safely override old rule
            replace rules rule
    ) oldRules
