module SyncBackup.Domain.Sync

open Microsoft.FSharp.Core
open SyncBackup
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

let private spreadRules (computeRule: 'a -> SyncRules -> Result<'a, string>) lastRule (rules: Rule list) =
    let rec spreadRules' (computeRule: 'a -> SyncRules -> Result<'a, string>) lastRule acc = function
        | [] -> Ok acc
        | rule: Rule::rules ->
            result {
                let subPathRules, others = rules |> List.partition (fun possibleChild -> RelativePath.contains possibleChild.Path rule.Path)
                let! appliedRule = computeRule lastRule rule.SyncRule
                let! childRules = spreadRules' computeRule appliedRule [] subPathRules
                let! otherRules = spreadRules' computeRule lastRule [] others
                return acc@[rule.Path, appliedRule]@childRules@otherRules
            }

    rules
    |> List.sortBy _.Path.Value
    |> spreadRules' computeRule lastRule []
    |> Result.map Map

let private spreadSourceRules (rules: Rule list) =
    let computeRule (lastRule: SourceRule) = function
        | Dsl.SyncRules.NoRule -> Ok lastRule
        | Dsl.SyncRules.Include -> Ok Include
        | Dsl.SyncRules.Exclude -> Ok Exclude
        | rule -> Error $"Rule \"{SyncRules.getValue rule}\" is not supposed to be setup in source repository."

    rules |> spreadRules computeRule Include

let private spreadBackupRules (rules: Rule list) =
    let computeRule (lastRule: BackupRule) = function
        | Dsl.SyncRules.NoRule -> Ok lastRule
        | Dsl.SyncRules.AlwaysReplace -> Ok Replace
        | Dsl.SyncRules.NotSave -> Ok NotSave
        | Dsl.SyncRules.NotDelete -> Ok NotDelete
        | rule -> Error $"Rule \"{SyncRules.getValue rule}\" is not supposed to be setup in backup repository."

    rules |> spreadRules computeRule Save

let private fullOuterJoin (sourceRules: Map<RelativePath, SourceRule>) (backupRules: Map<RelativePath, BackupRule>)  =
    [
        sourceRules |> Seq.map (fun x -> x.Key, Some x.Value, backupRules |> Map.tryFind x.Key)
        backupRules |> Seq.map (fun x -> x.Key, sourceRules |> Map.tryFind x.Key, Some x.Value)
    ]
    |> Seq.concat
    |> Seq.distinct
    |> Seq.sortBy (fun (path, _, _) -> path.Value)
    |> Seq.toList

let synchronize (sourceRules: Rule list) (backupRules: Rule list) =
    result {
        let! sourceRules = spreadSourceRules sourceRules
        let! backupRules = spreadBackupRules backupRules
        let spreadRules = fullOuterJoin sourceRules backupRules
        return spreadRules |> List.collect (function
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
    }
