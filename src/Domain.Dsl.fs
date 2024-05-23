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
