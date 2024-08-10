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


let private isUpdated source backup =
    match source, backup with
    | { LastWriteTime = Some x }, { LastWriteTime = Some y } when x > y -> true
    | _ -> false

let private ruleToContent (rule: Rule) = { Path = rule.Path; LastWriteTime = None }

module Synchronize =
    type private OriginRule = {
        Path: RelativePath
        SourceRule: SyncRules
        BackupRule: SyncRules
        IsUpdated: bool
    }

    type private SpreadRule = {
        Path: RelativePath
        SourceRule: SourceRule
        BackupRule: BackupRule
        IsUpdated: bool
    }

    let private buildTree
        (sourceItems: Content list)
        (sourceRules: Rule list)
        (backupItems: Content list)
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
            sourceItems@(sourceRules |> List.map ruleToContent)@backupItems@(backupRules |> List.map ruleToContent)
            |> List.distinctBy _.Path.Value

        let sourceItems = sourceItems |> List.map (fun x -> x.Path.Value, x) |> Map
        let sourceRules = sourceRules |> List.map (fun x -> x.Path.Value, x.SyncRule) |> Map
        let backupItems = backupItems |> List.map (fun x -> x.Path.Value, x) |> Map
        let backupRules = backupRules |> List.map (fun x -> x.Path.Value, x.SyncRule) |> Map

        paths
        |> Seq.collect (fun content ->
            let sourceItem = sourceItems |> Map.tryFind content.Path.Value
            let sourceRule = sourceRules |> Map.tryFind content.Path.Value
            let backupItem = backupItems |> Map.tryFind content.Path.Value
            let backupRule = backupRules |> Map.tryFind content.Path.Value

            match sourceItem, sourceRule, backupItem, backupRule with
            | None,         _,                  None,           _ ->                ([]: Item<OriginRule> list)
            | Some _,       None,               None,           None ->             [SourceItemOnly { Path = content.Path; SourceRule = NoRule; BackupRule = NoRule; IsUpdated = false }]
            | Some _,       Some sourceRule,    None,           None ->             [SourceItemOnly { Path = content.Path; SourceRule = sourceRule; BackupRule = NoRule; IsUpdated = false }]
            | Some _,       None,               None,           Some backupRule ->  [SourceItemOnly { Path = content.Path; SourceRule = NoRule; BackupRule = backupRule; IsUpdated = false }]
            | Some _,       Some sourceRule,    None,           Some backupRule ->  [SourceItemOnly { Path = content.Path; SourceRule = sourceRule; BackupRule = backupRule; IsUpdated = false }]
            | None,         None,               Some _,         None ->             [BackupItemOnly { Path = content.Path; SourceRule = NoRule; BackupRule = NoRule; IsUpdated = false }]
            | None,         Some sourceRule,    Some _,         None ->             [BackupItemOnly { Path = content.Path; SourceRule = sourceRule; BackupRule = NoRule; IsUpdated = false }]
            | None,         None,               Some _,         Some backupRule ->  [BackupItemOnly { Path = content.Path; SourceRule = NoRule; BackupRule = backupRule; IsUpdated = false }]
            | None,         Some sourceRule,    Some _,         Some backupRule ->  [BackupItemOnly { Path = content.Path; SourceRule = sourceRule; BackupRule = backupRule; IsUpdated = false }]
            | Some source,  None,               Some backup,    None ->             [BothItem { Path = content.Path; SourceRule = NoRule; BackupRule = NoRule; IsUpdated = isUpdated source backup }]
            | Some source,  Some sourceRule,    Some backup,    None ->             [BothItem { Path = content.Path; SourceRule = sourceRule; BackupRule = NoRule; IsUpdated = isUpdated source backup }]
            | Some source,  None,               Some backup,    Some backupRule ->  [BothItem { Path = content.Path; SourceRule = NoRule; BackupRule = backupRule; IsUpdated = isUpdated source backup }]
            | Some source,  Some sourceRule,    Some backup,    Some backupRule ->  [BothItem { Path = content.Path; SourceRule = sourceRule; BackupRule = backupRule; IsUpdated = isUpdated source backup }]
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
                    let itemsWithSpreadRules = treeItem.Element.map (fun origin -> { Path = origin.Path; SourceRule = appliedSourceRule; BackupRule = appliedBackupRule; IsUpdated = origin.IsUpdated })
                    let! otherItems = spreadRules' lastSourceRule lastBackupRule items
                    return [itemsWithSpreadRules]@childrenWithSpreadRules@otherItems
                }

        spreadRules' Include Save

    let private computeInstructions = function
        | SourceItemOnly { Path = path; SourceRule = Include;   BackupRule = Save }                             -> [InnerAdd path]
        | SourceItemOnly { Path = path; SourceRule = Include;   BackupRule = Replace }                          -> [InnerAdd path]
        | SourceItemOnly { Path = _;    SourceRule = Include;   BackupRule = NotSave }                          -> []
        | SourceItemOnly { Path = path; SourceRule = Include;   BackupRule = NotDelete }                        -> [InnerAdd path]
        | SourceItemOnly { Path = _;    SourceRule = Exclude;   BackupRule = _ }                                -> []

        | BackupItemOnly { Path = path; SourceRule = _;         BackupRule = Save }                             -> [InnerDelete path]
        | BackupItemOnly { Path = path; SourceRule = _;         BackupRule = Replace }                          -> [InnerDelete path]
        | BackupItemOnly { Path = path; SourceRule = _;         BackupRule = NotSave }                          -> [InnerDelete path]
        | BackupItemOnly { Path = path; SourceRule = _;         BackupRule = NotDelete }                        -> [InnerKeep path]

        | BothItem { Path = path;       SourceRule = Include;   BackupRule = Save;      IsUpdated = false }     -> [InnerKeep path]
        | BothItem { Path = path;       SourceRule = Include;   BackupRule = Save;      IsUpdated = true }      -> [InnerReplace path]
        | BothItem { Path = { ContentType = File } as path; SourceRule = Include; BackupRule = Replace }        -> [InnerReplace path]
        | BothItem { Path = { ContentType = Directory } as path; SourceRule = Include; BackupRule = Replace }   -> [InnerKeep path]
        | BothItem { Path = path;       SourceRule = Include;   BackupRule = NotSave }                          -> [InnerDelete path]
        | BothItem { Path = path;       SourceRule = Include;   BackupRule = NotDelete; IsUpdated = false }     -> [InnerKeep path]
        | BothItem { Path = path;       SourceRule = Include;   BackupRule = NotDelete; IsUpdated = true  }     -> [InnerReplace path]
        | BothItem { Path = path;       SourceRule = Exclude;   BackupRule = Save }                             -> [InnerDelete path]
        | BothItem { Path = path;       SourceRule = Exclude;   BackupRule = Replace }                          -> [InnerDelete path]
        | BothItem { Path = path;       SourceRule = Exclude;   BackupRule = NotSave }                          -> [InnerDelete path]
        | BothItem { Path = path;       SourceRule = Exclude;   BackupRule = NotDelete }                        -> [InnerKeep path]

    let run
        (sourceItems: Content list)
        (sourceRules: Rule list)
        (backupItems: Content list)
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
    type private OriginRule = {
        Path: RelativePath
        Rule: SyncRules
        IsUpdated: bool
    }

    type private SpreadRule = {
        Path: RelativePath
        BackupRule: BackupRule
        IsUpdated: bool
    }

    let private buildTree
        (rules: Rule list)
        (sourceItems: Content list)
        (backupItems: Content list) =
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
            sourceItems@(rules |> List.map ruleToContent)@backupItems
            |> List.distinctBy _.Path.Value

        let rules = rules |> List.map (fun x -> x.Path.Value, x.SyncRule) |> Map
        let sourceItems = sourceItems |> List.map (fun x -> x.Path.Value, x) |> Map
        let backupItems = backupItems |> List.map (fun x -> x.Path.Value, x) |> Map

        paths
        |> Seq.collect (fun content ->
            let sourceItem = sourceItems |> Map.tryFind content.Path.Value
            let rules = rules |> Map.tryFind content.Path.Value
            let backupItem = backupItems |> Map.tryFind content.Path.Value

            match sourceItem, backupItem, rules with
            | None,         None,           _ ->                ([]: Item<OriginRule> list)
            | Some _,       None,           None ->             [SourceItemOnly { Path = content.Path; Rule = NoRule; IsUpdated = false }]
            | Some _,       None,           Some backupRule ->  [SourceItemOnly { Path = content.Path; Rule = backupRule; IsUpdated = false }]
            | None,         Some _,         None ->             [BackupItemOnly { Path = content.Path; Rule = NoRule; IsUpdated = false }]
            | None,         Some _,         Some backupRule ->  [BackupItemOnly { Path = content.Path; Rule = backupRule; IsUpdated = false }]
            | Some source,  Some backup,    None ->             [BothItem { Path = content.Path; Rule = NoRule; IsUpdated = isUpdated source backup }]
            | Some source,  Some backup,    Some backupRule ->  [BothItem { Path = content.Path; Rule = backupRule; IsUpdated = isUpdated source backup }]
        )
        |> Seq.sortBy _.Item.Path.Value
        |> Seq.fold buildTree' []

    let private spreadRules =
        let computeRule (lastRule: BackupRule) = function
            | Dsl.SyncRules.NoRule -> Ok lastRule
            | Dsl.SyncRules.AlwaysReplace -> Ok Replace
            | Dsl.SyncRules.NotSave -> Ok NotSave
            | Dsl.SyncRules.NotDelete -> Ok NotDelete
            | rule -> Error $"Rule \"{SyncRules.getValue rule}\" is not supposed to be setup in backup repository."

        let correctRule (childrenRules: Item<SpreadRule> list) (rule: BackupRule) =
            match rule with
            | Save when childrenRules |> Seq.exists (fun child -> child.Item.BackupRule = NotDelete) -> NotDelete
            | _ -> rule

        let rec spreadRules' (lastRule: BackupRule) = function
            | [] -> Ok []
            | treeItem::items ->
                result {
                    let treeItem: Tree<OriginRule> = treeItem
                    let! appliedRule = computeRule lastRule treeItem.Element.Item.Rule
                    let! childrenWithSpreadRules = spreadRules' appliedRule treeItem.Children
                    let appliedRule = correctRule childrenWithSpreadRules appliedRule
                    let itemsWithSpreadRules = treeItem.Element.map (fun origin -> { Path = origin.Path; BackupRule = appliedRule; IsUpdated = origin.IsUpdated })
                    let! otherItems = spreadRules' lastRule items
                    return [itemsWithSpreadRules]@childrenWithSpreadRules@otherItems
                }

        spreadRules' Save

    let private computeInstructions = function
        | SourceItemOnly { Path = path; BackupRule = _ }                                    -> [InnerAdd path]

        | BackupItemOnly { Path = path; BackupRule = Save }                                 -> [InnerDelete path]
        | BackupItemOnly { Path = path; BackupRule = Replace }                              -> [InnerDelete path]
        | BackupItemOnly { Path = path; BackupRule = NotSave }                              -> [InnerDelete path]
        | BackupItemOnly { Path = path; BackupRule = NotDelete }                            -> [InnerKeep path]

        | BothItem { Path = { ContentType = File } as path; BackupRule = Replace }          -> [InnerReplace path]
        | BothItem { Path = { ContentType = Directory } as path; BackupRule = Replace }     -> [InnerKeep path]
        | BothItem { Path = path; BackupRule = _; IsUpdated = false }                       -> [InnerKeep path]
        | BothItem { Path = path; BackupRule = _; IsUpdated = true }                        -> [InnerReplace path]

    let run
        (rules: Rule list)
        (sourceItems: Content list)
        (backupItems: Content list) =
        buildTree rules sourceItems backupItems
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
