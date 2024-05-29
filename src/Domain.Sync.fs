module SyncBackup.Domain.Sync

open SyncBackup.Domain.Dsl

type SyncInstruction =
    | Add of RelativePath
    | Replace of RelativePath
    | Delete of RelativePath

type private SourceRule =
    | Include
    | Exclude

let private spreadSourceRules (rules: Rule list) =
    let computeRule (lastRule: SourceRule) = function
        | Dsl.SyncRules.NoRule -> lastRule
        | Dsl.SyncRules.Include -> Include
        | Dsl.SyncRules.Exclude -> Exclude
        |  _ -> failwith "not supposed to happen"

    let rec spreadSourceRules' lastRule acc = function
        | [] -> acc
        | rule: Rule::rules ->
            let subPathRules, others = rules |> List.partition (fun possibleChild -> RelativePath.contains possibleChild.Path rule.Path)
            let appliedRule = computeRule lastRule rule.SyncRule
            let childRules = spreadSourceRules' appliedRule [] subPathRules
            let otherRules = spreadSourceRules' lastRule [] others
            acc@[rule.Path, appliedRule]@childRules@otherRules

    rules
    |> List.sortBy _.Path.Value
    |> spreadSourceRules' Include []

let synchronize (sourceRules: Rule list) (backupRules: Rule list) =
    let sourceRules = spreadSourceRules sourceRules
    sourceRules
    |> List.collect (fun (path, syncRule) ->
        match syncRule with
        | Exclude -> []
        | Include -> [Add path]
    )
