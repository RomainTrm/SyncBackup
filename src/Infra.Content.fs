module SyncBackup.Infra.Content

open System
open System.IO
open Microsoft.FSharp.Core
open SyncBackup
open SyncBackup.Infra
open SyncBackup.Domain.Dsl

module Scan =
    let private excludeConfigFolder (directoryPath: string) =
        directoryPath.Contains Dsl.ConfigDirectory |> not

    let private sourceRelativePath (repositoryPath: RepositoryPath) fullPath (contentType: ContentType) =
        let relativePath = Path.GetRelativePath(repositoryPath, fullPath)
        { Value = relativePath; ContentType = contentType; Type = Source }

    let private aliasRelativePath (alias: Alias) fullPath (contentType: ContentType) =
        let relativePath = Path.GetRelativePath(alias.Path, fullPath)
        { Value = Path.Combine(alias.Name, relativePath); ContentType = contentType; Type = Alias }

    let rec private scan' (currentDirectoryPath: DirectoryPath) (buildRelativePath: string -> ContentType -> RelativePath) =
        let files =
            Directory.GetFiles currentDirectoryPath
            |> Seq.map (fun fullFilePath -> buildRelativePath fullFilePath ContentType.File)
            |> Seq.toList

        let directories =
            Directory.GetDirectories currentDirectoryPath
            |> Seq.filter excludeConfigFolder
            |> Seq.fold (fun acc directoryPath ->
                let path = buildRelativePath directoryPath ContentType.Directory
                let children =
                    try
                        scan' directoryPath buildRelativePath
                    with // Windows may detect a directory but fail to access it because it's not there
                    | :? UnauthorizedAccessException -> []
                acc@[path]@children
            ) []

        files@directories

    let run (repositoryPath: RepositoryPath) (aliases: Alias list) =
        let sourceDirectoryContent = scan' repositoryPath (sourceRelativePath repositoryPath)
        let aliasesDirectoriesContent = List.collect (fun (alias: Alias) -> scan' alias.Path (aliasRelativePath alias)) aliases
        sourceDirectoryContent@aliasesDirectoriesContent

module ScanFile =
    let rec private print scanResult =
        $"{ScanDiff.activeLine scanResult.Diff}{SyncRules.getValue scanResult.SyncRule} {ScanDiff.serialize scanResult.Diff} {RelativePath.serialize scanResult.Path}"

    let private buildFileContent repositoryType (rules: ScanResult list) =
        let fileLines = [
            "# Repository scan complete!"
            "# Use '#' to comment a line"
            $"# You can specify rules to every line (directories and files), by default '{SyncRules.getValue NoRule}' is set"
            "# Available rules as follows:"
            yield! SyncRules.getRulesAvailable repositoryType
                    |> Seq.map (SyncRules.getDescription >> sprintf "# - %s")
            ""
            yield! rules |> List.map print
            if rules = []
            then "# No change, your repository is up to date."
        ]
        String.Join (Dsl.NewLine, fileLines)

    let writeFile (repositoryPath: RepositoryPath) (repositoryType: RepositoryType) (rules: ScanResult list) =
        let fileContent = buildFileContent repositoryType rules
        let filePath = Dsl.getScanFileFilePath repositoryPath
        File.WriteAllText(filePath, fileContent)
        |> Ok

    let private removeComments (contentLine: string) =
        not (String.IsNullOrWhiteSpace contentLine || contentLine.StartsWith "#")

    let private parseSyncResult (contentLine: string) =
        match contentLine.Split ' ' |> Seq.toList with
        | rule::scanDiff::path ->
            result {
                let! rule = SyncRules.parse rule
                let! path = String.Join(' ', path) |> RelativePath.deserialize
                let! diff = ScanDiff.deserialize scanDiff
                return { SyncRule = rule; Path = path; Diff = diff }
            }
        | _ -> Error "Invalid format"

    let readFile (repositoryPath: RepositoryPath) =
        Dsl.getScanFileFilePath repositoryPath
        |> File.ReadAllLines
        |> Seq.filter removeComments
        |> Seq.map parseSyncResult
        |> Seq.fold (fun result content ->
            match result, content with
            | Ok result, Ok content -> Ok (result@[content])
            | _, Error error
            | Error error, _ -> Error error
        ) (Ok [])

module TrackFile =
    let save (repositoryPath: RepositoryPath) (contentPaths: RelativePath list) =
        let filePath = Dsl.getTrackFileFilePath repositoryPath
        let contentLines = contentPaths |> List.map RelativePath.serialize
        File.WriteAllLines (filePath, contentLines)
        Ok ()

    let load (repositoryPath: RepositoryPath) =
        let filePath = Dsl.getTrackFileFilePath repositoryPath
        if not (File.Exists filePath)
        then Ok []
        else
            File.ReadAllLines filePath
            |> Seq.fold (fun paths line ->
                paths
                |> Result.bind (fun paths ->
                    RelativePath.deserialize line
                    |> Result.map (fun path -> paths@[path])
                )
            ) (Ok [])
