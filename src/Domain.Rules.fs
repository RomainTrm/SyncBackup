module SyncBackup.Domain.Rules

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
