module SyncBackup.Domain.Dsl

type DirectoryPath = string
type RepositoryPath = DirectoryPath
type FilePath = string

type RepositoryConfig = {
    IsSourceRepository: bool
    Aliases: Alias list
}
and Alias = {
    Path: DirectoryPath
    Name: string
}
