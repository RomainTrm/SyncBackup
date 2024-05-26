module SyncBackup.Infra.Content

open System
open System.IO
open Microsoft.FSharp.Core
open SyncBackup.Infra
open SyncBackup.Domain.Dsl

module Scan =
    let private excludeConfigFolder (directoryPath: string) =
        directoryPath.Contains Dsl.ConfigDirectory |> not

    let private sourceRelativePath (repositoryPath: RepositoryPath) fullPath (contentType: ContentType) =
        let relativePath = Path.GetRelativePath(repositoryPath, fullPath)
        { Path = relativePath; ContentType = contentType; PathType = Source }

    let private aliasRelativePath (alias: Alias) fullPath (contentType: ContentType) =
        let relativePath = Path.GetRelativePath(alias.Path, fullPath)
        { Path = Path.Combine(alias.Name, relativePath); ContentType = contentType; PathType = Alias }

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

    let rec private printRule rule =
        $"{SyncRules.getValue rule.SyncRule} {RelativePath.printContentType rule.Path}\"{RelativePath.markAlias rule.Path}{rule.Path.Path}\""

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
        let buildPath (path: string list) = String.Join(' ', path).Replace("\"", "").Replace(RelativePath.AliasSymbol, "").Replace(RelativePath.FilePrefix, "").Replace(RelativePath.DirectoryPrefix, "")
        let buildRule path = SyncRules.parse >> Result.map (fun rule -> { SyncRule = rule; Path = path })

        match contentLine.Split ' ' |> Seq.toList with
        | [ _ ] -> Error "Invalid format"
        | rule::pathHead::pathTail when pathHead.StartsWith $"{RelativePath.FilePrefix}\"{RelativePath.AliasSymbol}" ->
            rule |> buildRule { Path = buildPath (pathHead::pathTail); ContentType = ContentType.File; PathType = Alias }
        | rule::pathHead::pathTail when pathHead.StartsWith $"{RelativePath.DirectoryPrefix}\"{RelativePath.AliasSymbol}" ->
            rule |> buildRule { Path = buildPath (pathHead::pathTail); ContentType = ContentType.Directory; PathType = Alias }
        | rule::pathHead::pathTail when pathHead.StartsWith RelativePath.FilePrefix ->
            rule |> buildRule { Path = buildPath (pathHead::pathTail); ContentType = ContentType.File; PathType = Source }
        | rule::pathHead::pathTail when pathHead.StartsWith RelativePath.DirectoryPrefix ->
            rule |> buildRule { Path = buildPath (pathHead::pathTail); ContentType = ContentType.Directory; PathType = Source }
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
        let contentLines =
            contentPaths
            |> List.map (fun relativePath -> $"{RelativePath.markAlias relativePath}{relativePath.Path}")
        File.WriteAllLines (filePath, contentLines)
        Ok ()
