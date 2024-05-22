module SyncBackup.Domain.Dsl

type DirectoryPath = string
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
and Directory = { Name: string; RelativePath: string; Content: Content list }
and File = { Name: string; RelativePath: string }
