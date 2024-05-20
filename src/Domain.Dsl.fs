module SyncBackup.Domain.Dsl

type DirectoryPath = string
type RepositoryPath = DirectoryPath
type FilePath = string

type RepositoryConfig = {
    IsMainRepository: bool
    Aliases: Alias list
}
and Alias = {
    Path: DirectoryPath
    Name: string
}
