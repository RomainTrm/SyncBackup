module SyncBackup.Domain.Rules

open SyncBackup.Domain.Dsl

type Rule = Dsl.Rule
type SyncRules = Dsl.SyncRules

type AddRuleResult =
    | Added of Rule list
    | Conflict of Rule * Rule
    | RuleAlreadyThere

let private rulesToAdd rule =
    [
        match rule.SyncRule with
        | NoRule -> ()
        | _ -> rule
    ]

let add (rule: Rule) (rules: Rule list) =
    if rules |> List.contains rule then RuleAlreadyThere
    elif rules |> List.exists (fun r -> r.Path = rule.Path)
    then
        let existingRule = rules |> List.find (fun r -> r.Path = rule.Path)
        Conflict (existingRule, rule)
    else
        match rulesToAdd rule with
        | [] -> RuleAlreadyThere
        | newRules -> Added (rules@newRules)

let replace (rules: Rule list) (rule: Rule) =
    let existingRule = rules |> List.find (fun r -> r.Path = rule.Path)
    rules
    |> List.except [existingRule]
    |> List.append (rulesToAdd rule)
