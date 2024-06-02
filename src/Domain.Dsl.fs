module SyncBackup.Domain.Dsl

type DirectoryPath = string
module DirectoryPath =
    let build (value: DirectoryPath) = value.Replace('/', '\\').TrimEnd [| '\\'; '\"' |]

type FilePath = string
/// Some path provided by the user, code doesn't known if it points to an element in the source or in an alias, neither if it's a directory or a file
type UnverifiedPath = string
/// Root path of the repository
type RepositoryPath = DirectoryPath

type RepositoryType = Source | Backup
type RepositoryConfig = {
    Type: RepositoryType
    Aliases: Alias list
    Rules: Rule list
}
and Alias = {
    Name: string
    Path: DirectoryPath
}
and Rule = {
    Path: RelativePath
    SyncRule: SyncRules
}
and RelativePath = {
    Value: RelativePathValue
    Type: PathType
    ContentType: ContentType
}
and RelativePathValue = string
and PathType = Source | Alias
and ContentType = Directory | File
and SyncRules =
    | NoRule
    | Exclude
    | Include
    | AlwaysReplace
    | NotSave
    | NotDelete
and ScanDiff =
    | AddedToRepository
    | RemovedFromRepository
    | RuleReminder
and ScanResult = {
    Path: RelativePath
    SyncRule: SyncRules
    Diff: ScanDiff
} with member this.Rule = { Path = this.Path; SyncRule = this.SyncRule }

module RelativePath =
    let [<Literal>] AliasSymbol = "*"
    let markAlias = function
        | { Type = Alias } -> AliasSymbol
        | { Type = Source } -> ""

    let [<Literal>] FilePrefix = "file::"
    let [<Literal>] DirectoryPrefix = "dir::"

    let printContentType = function
        | { ContentType = File } -> FilePrefix
        | { ContentType = Directory } -> DirectoryPrefix

    let serialize path = $"{printContentType path}\"{markAlias path}{path.Value}\""

    let deserialize (strValue: string) =
        let buildPath (path: string) =
            path.Replace("\"", "")
                .Replace(AliasSymbol, "")
                .Replace(FilePrefix, "")
                .Replace(DirectoryPrefix, "")

        match strValue with
        | _ when strValue.StartsWith $"{FilePrefix}\"{AliasSymbol}" ->
            Ok { Value = buildPath strValue; ContentType = ContentType.File; Type = Alias }
        | _ when strValue.StartsWith $"{DirectoryPrefix}\"{AliasSymbol}" ->
            Ok { Value = buildPath strValue; ContentType = ContentType.Directory; Type = Alias }
        | _ when strValue.StartsWith FilePrefix ->
            Ok { Value = buildPath strValue; ContentType = ContentType.File; Type = Source }
        | _ when strValue.StartsWith DirectoryPrefix ->
            Ok { Value = buildPath strValue; ContentType = ContentType.Directory; Type = Source }
        | _ -> Error "Invalid format"

    let contains child parent =
        match child, parent with
        | _ when child = parent -> false
        | { Type = childType }, { Type = parentType } when childType <> parentType -> false
        | { Value = childPath }, { Value = parentPath } -> childPath.StartsWith parentPath

module SyncRules =
    let getValue = function
        | NoRule -> "norule"
        | Exclude -> "exclude"
        | Include -> "include"
        | AlwaysReplace -> "replace"
        | NotSave -> "ignore"
        | NotDelete -> "preserve"

    let getDescription = function
        | NoRule -> $"{getValue NoRule} (default): no specific rule specified for this directory or file. Rules will be inherited, if no rule specified, then it will be included to backup."
        | Exclude -> $"{getValue Exclude}: this directory or file must not be saved into backup, even if parents are included."
        | Include -> $"{getValue Include}: this directory or file must be saved into backup, even if parents are excluded."
        | AlwaysReplace -> $"{getValue AlwaysReplace}: this directory or file is always be replaced, even if it already exists in backup."
        | NotSave -> $"{getValue NotSave}: this directory or file must not be saved into backup, even if present in the source repository."
        | NotDelete -> $"{getValue NotDelete}: this directory or file must not be deleted from backup, even if the source repository does not include it."

    let getRulesAvailable = function
        | RepositoryType.Source -> [NoRule; Exclude; Include]
        | RepositoryType.Backup -> [NoRule; AlwaysReplace; NotSave; NotDelete]

    let parse = function
        | "norule" -> Ok NoRule
        | "exclude" -> Ok Exclude
        | "include" -> Ok Include
        | "replace" -> Ok AlwaysReplace
        | "ignore" -> Ok NotSave
        | "preserve" -> Ok NotDelete
        | _ -> Error "Invalid rule"

module ScanResult =
    let build diff (rule: Rule) = {
        Path = rule.Path
        SyncRule = rule.SyncRule
        Diff = diff
    }

module ScanDiff =
    let serialize = function
        | AddedToRepository -> "(added)"
        | RemovedFromRepository -> "(removed)"
        | RuleReminder -> "(nochange)"

    let deserialize = function
        | "(added)" -> Ok AddedToRepository
        | "(removed)" -> Ok RemovedFromRepository
        | "(nochange)" -> Ok RuleReminder
        | _ -> Error "Invalid diff"

    let activeLine = function
        | AddedToRepository -> ""
        | RemovedFromRepository -> ""
        | RuleReminder -> "# "
