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

module Synchronize =
    type private SourceRule =
        | Include
        | Exclude

    type private BackupRule =
        | Save
        | Replace
        | NotSave
        | NotDelete

    type private InnerSyncInstruction =
        | InnerAdd of RelativePath
        | InnerReplace of RelativePath
        | InnerDelete of RelativePath
        | InnerKeep of RelativePath

    type private Item<'a> =
        | SourceItemOnly of 'a
        | BackupItemOnly of 'a
        | BothItem of 'a
    with
        member this.Item =
            match this with
            | SourceItemOnly item -> item
            | BackupItemOnly item -> item
            | BothItem item -> item

        member this.map f =
            match this with
            | SourceItemOnly item -> SourceItemOnly (f item)
            | BackupItemOnly item -> BackupItemOnly (f item)
            | BothItem item -> BothItem (f item)

    type private Tree<'a> = {
        Element: Item<'a>
        Children: Tree<'a> list
    }

    type private OriginRule = {
        Path: RelativePath
        SourceRule: SyncRules
        BackupRule: SyncRules
    }

    type private SpreadRule = {
        Path: RelativePath
        SourceRule: SourceRule
        BackupRule: BackupRule
    }

    let private buildTree
        (sourceItems: RelativePath list)
        (sourceRules: Rule list)
        (backupItems: RelativePath list)
        (backupRules: Rule list) =
        let rec buildTree' (tree: Tree<OriginRule> list) (item: Item<OriginRule>) =
            if tree |> List.exists (fun treeItem -> RelativePath.contains item.Item.Path treeItem.Element.Item.Path)
            then
                tree
                |> List.map (fun treeItem ->
                    if RelativePath.contains item.Item.Path treeItem.Element.Item.Path
                    then { treeItem with Children = buildTree' treeItem.Children item }
                    else treeItem
                )
            else { Element = item; Children = [] }::tree

        let paths =
            sourceItems@(sourceRules |> List.map _.Path)@backupItems@(backupRules |> List.map _.Path)
            |> List.distinctBy _.Value

        let sourceItems = sourceItems |> List.map (fun x -> x.Value, x) |> Map
        let sourceRules = sourceRules |> List.map (fun x -> x.Path.Value, x.SyncRule) |> Map
        let backupItems = backupItems |> List.map (fun x -> x.Value, x) |> Map
        let backupRules = backupRules |> List.map (fun x -> x.Path.Value, x.SyncRule) |> Map

        paths
        |> Seq.collect (fun path ->
            let sourceItem = sourceItems |> Map.containsKey path.Value
            let sourceRule = sourceRules |> Map.tryFind path.Value
            let backupItem = backupItems |> Map.containsKey path.Value
            let backupRule = backupRules |> Map.tryFind path.Value

            match sourceItem, sourceRule, backupItem, backupRule with
            | false, _, false, _ -> ([]: Item<OriginRule> list)
            | true, None, false, None -> [SourceItemOnly { Path = path; SourceRule = NoRule; BackupRule = NoRule }]
            | true, Some sourceRule, false, None -> [SourceItemOnly { Path = path; SourceRule = sourceRule; BackupRule = NoRule }]
            | true, None, false, Some backupRule -> [SourceItemOnly { Path = path; SourceRule = NoRule; BackupRule = backupRule }]
            | true, Some sourceRule, false, Some backupRule -> [SourceItemOnly { Path = path; SourceRule = sourceRule; BackupRule = backupRule }]
            | false, None, true, None -> [BackupItemOnly { Path = path; SourceRule = NoRule; BackupRule = NoRule }]
            | false, Some sourceRule, true, None -> [BackupItemOnly { Path = path; SourceRule = sourceRule; BackupRule = NoRule }]
            | false, None, true, Some backupRule -> [BackupItemOnly { Path = path; SourceRule = NoRule; BackupRule = backupRule }]
            | false, Some sourceRule, true, Some backupRule -> [BackupItemOnly { Path = path; SourceRule = sourceRule; BackupRule = backupRule }]
            | true, None, true, None -> [BothItem { Path = path; SourceRule = NoRule; BackupRule = NoRule }]
            | true, Some sourceRule, true, None -> [BothItem { Path = path; SourceRule = sourceRule; BackupRule = NoRule }]
            | true, None, true, Some backupRule -> [BothItem { Path = path; SourceRule = NoRule; BackupRule = backupRule }]
            | true, Some sourceRule, true, Some backupRule -> [BothItem { Path = path; SourceRule = sourceRule; BackupRule = backupRule }]
        )
        |> Seq.sortBy _.Item.Path.Value
        |> Seq.fold buildTree' []

    let private spreadRules =
        let computeSourceRule (lastRule: SourceRule) = function
            | Dsl.SyncRules.NoRule -> Ok lastRule
            | Dsl.SyncRules.Include -> Ok Include
            | Dsl.SyncRules.Exclude -> Ok Exclude
            | rule -> Error $"Rule \"{SyncRules.getValue rule}\" is not supposed to be setup in source repository."

        let computeBackupRule (lastRule: BackupRule) = function
            | Dsl.SyncRules.NoRule -> Ok lastRule
            | Dsl.SyncRules.AlwaysReplace -> Ok Replace
            | Dsl.SyncRules.NotSave -> Ok NotSave
            | Dsl.SyncRules.NotDelete -> Ok NotDelete
            | rule -> Error $"Rule \"{SyncRules.getValue rule}\" is not supposed to be setup in backup repository."

        let computeRule (lastSourceRule: SourceRule) (lastBackupRule: BackupRule) (item: OriginRule) : Result<SourceRule * BackupRule, string> =
            result {
                let! sourceRule = computeSourceRule lastSourceRule item.SourceRule
                let! backupRule = computeBackupRule lastBackupRule item.BackupRule
                return sourceRule, backupRule
            }

        let correctRule (childrenRules: Item<SpreadRule> list) (sourceRule: SourceRule) (backupRule: BackupRule) =
            match sourceRule, backupRule with
            | Exclude, _ when childrenRules |> Seq.exists (fun child -> child.Item.SourceRule = Include) -> Include, backupRule
            | _, NotSave when childrenRules |> Seq.exists (fun child -> child.Item.BackupRule <> NotSave) -> sourceRule, NotDelete
            | _, Save when childrenRules |> Seq.exists (fun child -> child.Item.BackupRule = NotDelete) -> sourceRule, NotDelete
            | _ -> sourceRule, backupRule

        let rec spreadRules' (lastSourceRule: SourceRule) (lastBackupRule: BackupRule) = function
            | [] -> Ok []
            | treeItem::items ->
                result {
                    let treeItem: Tree<OriginRule> = treeItem
                    let! appliedSourceRule, appliedBackupRule = computeRule lastSourceRule lastBackupRule treeItem.Element.Item
                    let! childrenWithSpreadRules = spreadRules' appliedSourceRule appliedBackupRule treeItem.Children
                    let appliedSourceRule, appliedBackupRule = correctRule childrenWithSpreadRules appliedSourceRule appliedBackupRule
                    let itemsWithSpreadRules = treeItem.Element.map (fun origin -> { Path = origin.Path; SourceRule = appliedSourceRule; BackupRule = appliedBackupRule })
                    let! otherItems = spreadRules' lastSourceRule lastBackupRule items
                    return [itemsWithSpreadRules]@childrenWithSpreadRules@otherItems
                }

        spreadRules' Include Save

    let private computeInstructions = function
        | SourceItemOnly { Path = path; SourceRule = Include; BackupRule = Save } -> [InnerAdd path]
        | SourceItemOnly { Path = path; SourceRule = Include; BackupRule = Replace } -> [InnerAdd path]
        | SourceItemOnly { Path = _; SourceRule = Include; BackupRule = NotSave } -> []
        | SourceItemOnly { Path = path; SourceRule = Include; BackupRule = NotDelete } -> [InnerAdd path]
        | SourceItemOnly { Path = _; SourceRule = Exclude; BackupRule = _ } -> []

        | BackupItemOnly { Path = path; SourceRule = _; BackupRule = Save } -> [InnerDelete path]
        | BackupItemOnly { Path = path; SourceRule = _; BackupRule = Replace } -> [InnerDelete path]
        | BackupItemOnly { Path = path; SourceRule = _; BackupRule = NotSave } -> [InnerDelete path]
        | BackupItemOnly { Path = path; SourceRule = _; BackupRule = NotDelete } -> [InnerKeep path]

        | BothItem { Path = path; SourceRule = Include; BackupRule = Save } -> [InnerKeep path]
        | BothItem { Path = { ContentType = File } as path; SourceRule = Include; BackupRule = Replace } -> [InnerReplace path]
        | BothItem { Path = { ContentType = Directory } as path; SourceRule = Include; BackupRule = Replace } -> [InnerKeep path]
        | BothItem { Path = path; SourceRule = Include; BackupRule = NotSave } -> [InnerDelete path]
        | BothItem { Path = path; SourceRule = Include; BackupRule = NotDelete } -> [InnerKeep path]
        | BothItem { Path = path; SourceRule = Exclude; BackupRule = Save } -> [InnerDelete path]
        | BothItem { Path = path; SourceRule = Exclude; BackupRule = Replace } -> [InnerDelete path]
        | BothItem { Path = path; SourceRule = Exclude; BackupRule = NotSave } -> [InnerDelete path]
        | BothItem { Path = path; SourceRule = Exclude; BackupRule = NotDelete } -> [InnerKeep path]

    let private (|Instruction|) = function
        | InnerAdd path
        | InnerReplace path
        | InnerKeep path
        | InnerDelete path -> path

    let private orderInstructions left right =
        match left, right with
        | InnerDelete left, InnerDelete right when left |> RelativePath.contains right -> 1
        | InnerDelete left, InnerDelete right when right |> RelativePath.contains left -> -1
        | InnerAdd left, InnerAdd right when left |> RelativePath.contains right -> -1
        | InnerAdd left, InnerAdd right when right |> RelativePath.contains left -> 1
        | Instruction left, Instruction right -> compare left right

    let run
        (sourceItems: RelativePath list)
        (sourceRules: Rule list)
        (backupItems: RelativePath list)
        (backupRules: Rule list) =
        buildTree sourceItems sourceRules backupItems backupRules
        |> spreadRules
        |> Result.map (
            List.collect computeInstructions
            >> List.sortWith orderInstructions
            >> List.collect (function
                | InnerKeep _ -> []
                | InnerAdd path -> [Add path]
                | InnerReplace path -> [SyncInstruction.Replace path]
                | InnerDelete path -> [Delete path]
            )
        )

module Replicate =
    let run
        (rules: Rule list)
        (sourceItems: RelativePath list)
        (backupItems: RelativePath list) =
        Set backupItems
        |> Set.difference (Set sourceItems)
        |> Set.toList
        |> List.map Add
        |> List.sortWith (fun l r ->
            match l, r with
            | Add left, Add right when left |> RelativePath.contains right -> -1
            | Add left, Add right when right |> RelativePath.contains left -> 1
            | Add left, Add right -> compare left right
        )
        |> Ok
