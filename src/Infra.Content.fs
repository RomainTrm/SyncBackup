module SyncBackup.Infra.Content

open System.IO
open SyncBackup.Infra
open SyncBackup.Domain.Dsl

let scan (repositoryPath: RepositoryPath) (aliases: Alias list) =
    let rec scan' (directorySourcePath: DirectoryPath) (currentDirectoryPath: DirectoryPath) (relativePath: string -> RelativePath) acc =
        let files =
            Directory.GetFiles currentDirectoryPath
            |> Seq.map Path.GetFileName
            |> Seq.map (fun fileName ->
                let fullFilePath = Path.Combine(currentDirectoryPath, fileName)
                let relativeFilePath = Path.GetRelativePath(directorySourcePath, fullFilePath)
                File { Name = fileName; RelativePath = relativePath relativeFilePath }
            )
            |> Seq.toList

        let directories =
            Directory.GetDirectories currentDirectoryPath
            |> Seq.filter (fun directoryPath -> directoryPath.Contains Dsl.ConfigDirectory |> not)
            |> Seq.fold (fun acc directoryPath ->
                let directoryContent = scan' directorySourcePath directoryPath relativePath []
                let relativeDirectoryPath = Path.GetRelativePath(directorySourcePath, directoryPath)
                let directory = Directory { Name = Path.GetFileName directoryPath; RelativePath = relativePath relativeDirectoryPath; Content = directoryContent }
                acc@[directory]
            ) []

        acc@files@directories

    let sourceDirectoryContent = scan' repositoryPath repositoryPath Source []
    let aliasesDirectoriesContent =
        aliases
        |> List.collect (fun alias ->
            let aliasRelativePath path = Alias (Path.Combine(alias.Name, path))
            scan' alias.Path alias.Path aliasRelativePath []
        )
    sourceDirectoryContent@aliasesDirectoriesContent
