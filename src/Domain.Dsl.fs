module SyncBackup.Domain.Dsl

type DirectoryPath = string
module DirectoryPath =
    let build (value: DirectoryPath) = value.TrimEnd [| '\\'; '\"' |]

type FilePath = string
/// Some path provided by the user, code doesn't known if it points to an element in the source or in an alias, neither if it's a directory or a file
type UnverifiedPath = string
/// Root path of the repository
type RepositoryPath = DirectoryPath

type RepositoryConfig = {
    IsSourceRepository: bool
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
    Value: string
    Type: PathType
    ContentType: ContentType
}
and PathType = Source | Alias
and ContentType = Directory | File
and SyncRules =
    | NoRule
    | Exclude
    | Include
and ScanDiff =
    | AddedToRepository
    | RemovedFromRepository
    | RuleReminder
and ScanResult = Rule * ScanDiff

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
        | _ when child = parent -> true
        | { Type = childType }, { Type = parentType } when childType <> parentType -> false
        | { Value = childPath }, { Value = parentPath } -> childPath.StartsWith parentPath

module SyncRules =
    let getValue = function
        | NoRule -> "norule"
        | Exclude -> "exclude"
        | Include -> "include"

    let getDescription = function
        | NoRule -> $"{getValue NoRule} (default): no specific rule specified for this directory or file. Rules will be inherited, if no rule specified, then it will be included to backup."
        | Exclude -> $"{getValue Exclude}: this directory or file must not be saved into backup, even if parents are included."
        | Include -> $"{getValue Include}: this directory or file must be saved into backup, even if parents are excluded."

    let parse = function
        | "norule" -> Ok NoRule
        | "exclude" -> Ok Exclude
        | "include" -> Ok Include
        | _ -> Error "Invalid rule"

module ScanDiff =
    let serialize = function
        | AddedToRepository -> "(added)"
        | RemovedFromRepository -> "(removed)"
        | RuleReminder -> "(nochange)"
