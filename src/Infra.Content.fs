module SyncBackup.Infra.Content

open System.IO
open SyncBackup.Infra
open SyncBackup.Domain.Dsl

let scan (repositoryPath: RepositoryPath) (aliases: Alias list) =
    let rec scan' currentDirectoryPath acc =
        let files =
            Directory.GetFiles currentDirectoryPath
            |> Seq.map Path.GetFileName
            |> Seq.map (fun fileName ->
                let fullFilePath = Path.Combine(currentDirectoryPath, fileName)
                let relativeFilePath = Path.GetRelativePath(repositoryPath, fullFilePath)
                File { Name = fileName; RelativePath = relativeFilePath }
            )
            |> Seq.toList

        let directories =
            Directory.GetDirectories currentDirectoryPath
            |> Seq.filter (fun directoryPath -> directoryPath.Contains Dsl.ConfigDirectory |> not)
            |> Seq.fold (fun acc directoryPath ->
                let directoryContent = scan' directoryPath []
                let relativeDirectoryPath = Path.GetRelativePath(repositoryPath, directoryPath)
                let directory = Directory { Name = Path.GetFileName directoryPath; RelativePath = relativeDirectoryPath; Content = directoryContent }
                acc@[directory]
            ) []

        acc@files@directories

    scan' repositoryPath []

