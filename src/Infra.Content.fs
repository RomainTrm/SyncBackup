module SyncBackup.Infra.Content

open System.IO
open SyncBackup.Infra
open SyncBackup.Domain.Dsl

module Scan =
    let private excludeConfigFolder (directoryPath: string) =
        directoryPath.Contains Dsl.ConfigDirectory |> not

    let private sourceRelativePath (repositoryPath: RepositoryPath) fullPath =
        let relativePath = Path.GetRelativePath(repositoryPath, fullPath)
        Source relativePath

    let private aliasRelativePath (alias: Alias) fullPath =
        let relativePath = Path.GetRelativePath(alias.Path, fullPath)
        Alias (Path.Combine(alias.Name, relativePath))

    let rec private scan' (currentDirectoryPath: DirectoryPath) (buildRelativePath: string -> RelativePath) =
        let files =
            Directory.GetFiles currentDirectoryPath
            |> Seq.map (fun fullFilePath -> File {
                Name = Path.GetFileName fullFilePath
                RelativePath = buildRelativePath fullFilePath
            })
            |> Seq.toList

        let directories =
            Directory.GetDirectories currentDirectoryPath
            |> Seq.filter excludeConfigFolder
            |> Seq.fold (fun acc directoryPath ->
                acc@[Directory {
                    Name = Path.GetFileName directoryPath
                    RelativePath = buildRelativePath directoryPath
                    Content = scan' directoryPath buildRelativePath
                }]
            ) []

        files@directories

    let run (repositoryPath: RepositoryPath) (aliases: Alias list) =
        let sourceDirectoryContent = scan' repositoryPath (sourceRelativePath repositoryPath)
        let aliasesDirectoriesContent = List.collect (fun alias -> scan' alias.Path (aliasRelativePath alias)) aliases
        sourceDirectoryContent@aliasesDirectoriesContent
