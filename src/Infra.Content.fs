﻿module SyncBackup.Infra.Content

open System
open System.IO
open Microsoft.FSharp.Core
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
        let aliasesDirectoriesContent = List.collect (fun (alias: Alias) -> scan' alias.Path (aliasRelativePath alias)) aliases
        sourceDirectoryContent@aliasesDirectoriesContent

module ScanFile =
    open Microsoft.FSharp.Reflection

    let rec private printRule rule =
        $"{SyncRules.getValue rule.SyncRule} \"{RelativePath.markAlias rule.Path}{ RelativePath.getPath rule.Path}\""

    let private buildFileContent rules =
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

    let writeFile (repositoryPath: RepositoryPath) (rules: Rule list) =
        let fileContent = buildFileContent rules
        let filePath = Dsl.getScanFileFilePath repositoryPath
        File.WriteAllText(filePath, fileContent)
        |> Ok

    let private removeComments (contentLine: string) =
        not (String.IsNullOrWhiteSpace contentLine || contentLine.StartsWith "#")

    let private parseRule (contentLine: string) =
        let buildPath pathCtor (path: string list) = pathCtor (String.Join(' ', path).Replace("\"", "").Replace("*", ""))
        let buildRule path = SyncRules.parse >> Result.map (fun rule -> { SyncRule = rule; Path = path })

        match contentLine.Split ' ' |> Seq.toList with
        | [ _ ] -> Error "Invalid format"
        | rule::pathHead::pathTail when pathHead.StartsWith "\"*" ->
            rule |> buildRule (buildPath Alias (pathHead::pathTail))
        | rule::path -> rule |> buildRule (buildPath Source path)
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
    let save (repositoryPath: RepositoryPath) (content: RelativePath list) =
        let filePath = Dsl.getTrackFileFilePath repositoryPath
        let contentLines =
            content
            |> List.map (fun content -> $"{RelativePath.markAlias content}{RelativePath.getPath content}")
        File.WriteAllLines (filePath, contentLines)
        Ok ()
