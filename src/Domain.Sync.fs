module SyncBackup.Domain.Sync

open SyncBackup.Domain.Dsl

type SyncInstruction =
    | Add of RelativePath
    | Replace of RelativePath
    | Delete of RelativePath

type private SourceRule =
    | Include
    | Exclude

type private BackupRule =
    | Save
    | Replace
    | NotSave
    | NotDelete

let rec private spreadRules computeRule lastRule acc = function
    | [] -> acc
    | rule: Rule::rules ->
        let subPathRules, others = rules |> List.partition (fun possibleChild -> RelativePath.contains possibleChild.Path rule.Path)
        let appliedRule = computeRule lastRule rule.SyncRule
        let childRules = spreadRules computeRule appliedRule [] subPathRules
        let otherRules = spreadRules computeRule lastRule [] others
        acc@[rule.Path, appliedRule]@childRules@otherRules

let private spreadSourceRules (rules: Rule list) =
    let computeRule (lastRule: SourceRule) = function
        | Dsl.SyncRules.NoRule -> lastRule
        | Dsl.SyncRules.Include -> Include
        | Dsl.SyncRules.Exclude -> Exclude
        |  _ -> failwith "not supposed to happen"

    rules
    |> List.sortBy _.Path.Value
    |> spreadRules computeRule Include []

let private spreadBackupRules (rules: Rule list) =
    let computeRule (lastRule: BackupRule) = function
        | Dsl.SyncRules.NoRule -> lastRule
        | Dsl.SyncRules.AlwaysReplace -> Replace
        | Dsl.SyncRules.NotSave -> NotSave
        | Dsl.SyncRules.NotDelete -> NotDelete
        |  _ -> failwith "not supposed to happen"

    rules
    |> List.sortBy _.Path.Value
    |> spreadRules computeRule Save []

let private fullOuterJoin (sourceRules: (RelativePath * SourceRule) list) (backupRules: (RelativePath * BackupRule) list)  =
    let sourceRulesMap = sourceRules |> List.map (fun x -> fst x, snd x) |> Map
    let backupRulesMap = backupRules |> List.map (fun x -> fst x, snd x) |> Map
    [
        sourceRulesMap |> Seq.map (fun x -> x.Key, Some x.Value, backupRulesMap |> Map.tryFind x.Key)
        backupRulesMap |> Seq.map (fun x -> x.Key, sourceRulesMap |> Map.tryFind x.Key, Some x.Value)
    ]
    |> Seq.concat
    |> Seq.distinct
    |> Seq.sortBy (fun (path, _, _) -> path.Value)
    |> Seq.toList

let synchronize (sourceRules: Rule list) (backupRules: Rule list) =
    let sourceRules = spreadSourceRules sourceRules
    let backupRules = spreadBackupRules backupRules
    let spreadRules = fullOuterJoin sourceRules backupRules

    spreadRules
    |> List.collect (function
        | _, None, None -> []
        | path, Some Include, None -> [Add path]
        | _, Some Include, Some Save -> []
        | path, Some Include, Some Replace -> [SyncInstruction.Replace path]
        | _, Some Include, Some NotSave -> []
        | _, Some Include, Some NotDelete -> []
        | _, Some Exclude, None -> []
        | path, Some Exclude, Some Save -> [Delete path]
        | path, Some Exclude, Some Replace -> [Delete path]
        | path, Some Exclude, Some NotSave -> [Delete path]
        | _, Some Exclude, Some NotDelete -> []
        | _, None, Some NotDelete -> []
        | path, None, Some Save -> [Delete path]
        | path, None, Some NotSave -> [Delete path]
        | path, None, Some Replace -> [Delete path]
    )
