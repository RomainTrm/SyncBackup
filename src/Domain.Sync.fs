module SyncBackup.Domain.Sync

open Microsoft.FSharp.Core
open SyncBackup
open SyncBackup.Domain.Dsl

type SyncInstruction =
    | Add of RelativePath
    | Replace of RelativePath
    | Delete of RelativePath

module SyncInstruction =
    let serialize = function
        | Add path -> $"- Add: {RelativePath.serialize path}"
        | Replace path -> $"- Replace: {RelativePath.serialize path}"
        | Delete path -> $"- Delete: {RelativePath.serialize path}"

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
                return acc@[rule.Path.Value, (rule.Path, appliedRule)]@childRules@otherRules
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

let private fullOuterJoin (sourceRules: Map<RelativePathValue, RelativePath * SourceRule>) (backupRules: Map<RelativePathValue, RelativePath * BackupRule>)  =
    [
        sourceRules |> Seq.map (fun x ->
            fst x.Value,
            Some (snd x.Value),
            backupRules |> Map.tryFind x.Key |> Option.map snd
        )
        backupRules |> Seq.map (fun x ->
            fst x.Value,
            sourceRules |> Map.tryFind x.Key |> Option.map snd,
            Some (snd x.Value)
        )
    ]
    |> Seq.concat
    |> Seq.distinctBy (fun (path, _, _) -> path.Value)
    |> Seq.sortBy (fun (path, _, _) -> path.Value)
    |> Seq.toList

let private computeInstructions = function
    | _, None, None -> []
    | path, Some Include, None -> [Add path]
    | _, Some Include, Some Save -> []
    | { ContentType = File } as path, Some Include, Some Replace -> [SyncInstruction.Replace path]
    | { ContentType = Directory }, Some Include, Some Replace -> []
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

let private (|Instruction|) = function
    | Add path
    | SyncInstruction.Replace path
    | Delete path -> Some path

let private orderInstructions left right =
    match left, right with
    | Delete left, Delete right when left |> RelativePath.contains right -> 1
    | Delete left, Delete right when right |> RelativePath.contains left -> -1
    | Add left, Add right when left |> RelativePath.contains right -> -1
    | Add left, Add right when right |> RelativePath.contains left -> 1
    | Instruction left, Instruction right -> compare left right

let synchronize (sourceRules: Rule list) (backupRules: Rule list) =
    result {
        let! sourceRules = spreadSourceRules sourceRules
        let! backupRules = spreadBackupRules backupRules
        return
            fullOuterJoin sourceRules backupRules
            |> List.collect computeInstructions
            |> List.sortWith orderInstructions
    }
