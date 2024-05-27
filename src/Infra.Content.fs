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
                acc@[path]@(scan' directoryPath buildRelativePath)
            ) []

        files@directories

    let run (repositoryPath: RepositoryPath) (aliases: Alias list) =
        let sourceDirectoryContent = scan' repositoryPath (sourceRelativePath repositoryPath)
        let aliasesDirectoriesContent = List.collect (fun (alias: Alias) -> scan' alias.Path (aliasRelativePath alias)) aliases
        sourceDirectoryContent@aliasesDirectoriesContent

module ScanFile =
    open Microsoft.FSharp.Reflection

    let rec private printRule (rule, diff) =
        $"{ScanDiff.activeLine diff}{SyncRules.getValue rule.SyncRule} {ScanDiff.serialize diff} {RelativePath.serialize rule.Path}"

    let private buildFileContent (rules: ScanResult list) =
        let fileLines = [
            "# Repository scan complete!"
            "# Use '#' to comment a line"
            $"# You can specify rules to every line (directories and files), by default '{SyncRules.getValue NoRule}' is set"
            "# Available rules as follows:"
            yield! FSharpType.GetUnionCases(typeof<SyncRules>)
                    |> Seq.map (fun rule -> FSharpValue.MakeUnion(rule, [||]) :?> SyncRules)
                    |> Seq.map (SyncRules.getDescription >> sprintf "# - %s")
            ""
            yield! rules |> List.map printRule
        ]
        String.Join (Dsl.NewLine, fileLines)

    let writeFile (repositoryPath: RepositoryPath) (rules: ScanResult list) =
        let fileContent = buildFileContent rules
        let filePath = Dsl.getScanFileFilePath repositoryPath
        File.WriteAllText(filePath, fileContent)
        |> Ok

    let private removeComments (contentLine: string) =
        not (String.IsNullOrWhiteSpace contentLine || contentLine.StartsWith "#")

    let private parseRule (contentLine: string) =
        match contentLine.Split ' ' |> Seq.toList with
        | rule::_::path ->
            result {
                let! rule = SyncRules.parse rule
                let! path = String.Join(' ', path) |> RelativePath.deserialize
                return { SyncRule = rule; Path = path }
            }
        | _ -> Error "Invalid format"

    let readFile (repositoryPath: RepositoryPath) () =
        Dsl.getScanFileFilePath repositoryPath
        |> File.ReadAllLines
        |> Seq.filter removeComments
        |> Seq.map parseRule
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
