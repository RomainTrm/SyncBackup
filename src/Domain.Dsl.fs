module SyncBackup.Domain.Dsl

type DirectoryPath = string
module DirectoryPath =
    let build (value: string) = value.TrimEnd [| '\\'; '\"' |]

type RepositoryPath = DirectoryPath
type FilePath = string

type RepositoryConfig = {
    IsSourceRepository: bool
    Aliases: Alias list
}
and Alias = {
    Name: string
    Path: DirectoryPath
}

type Content =
    | Directory of Directory
    | File of File
and Directory = { Name: string; RelativePath: RelativePath; Content: Content list }
and File = { Name: string; RelativePath: RelativePath }
and RelativePath =
    | Source of string
    | Alias of string

type SyncRules =
    | NoRule
    | Exclude
    | Include

module SyncRules =
    let getValue = function
        | NoRule -> "norule"
        | Exclude -> "exclude"
        | Include -> "include"

    let getDescription = function
        | NoRule -> $"{getValue NoRule} (default): no specific rule specified for this directory or file. Rules will be inherited, if no rule specified, then it will be included to backup."
        | Exclude -> $"{getValue Exclude}: this directory or file must not be saved into backup, even if parents are included."
        | Include -> $"{getValue Include}: this directory or file must be saved into backup, even if parents are excluded."
