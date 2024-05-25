module SyncBackup.Infra.Content

open System
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
        let aliasesDirectoriesContent = List.collect (fun (alias: Alias) -> scan' alias.Path (aliasRelativePath alias)) aliases
        sourceDirectoryContent@aliasesDirectoriesContent

module ScanFile =
    open Microsoft.FSharp.Reflection

    let rec private printContent = function
        | Directory { RelativePath = relativePath; Content = content } ->
            [
                $"{SyncRules.getValue NoRule} (directory) \"{RelativePath.getPath relativePath}\""
                yield! content |> List.collect printContent
            ]
        | File { RelativePath = relativePath } -> [ $"{SyncRules.getValue NoRule} (file) \"{ RelativePath.getPath relativePath}\"" ]

    let private buildFileContent content =
        let fileLines = [
            "# Repository scan complete!"
            "# Use '#' to comment a line"
            $"# You can specify rules to every line (directories and files), by default '{SyncRules.getValue NoRule}' is set"
            "# Available rules as follows:"
            yield! FSharpType.GetUnionCases(typeof<SyncRules>)
                    |> Seq.map (fun rule -> FSharpValue.MakeUnion(rule, [||]) :?> SyncRules)
                    |> Seq.map (SyncRules.getDescription >> sprintf "# - %s")
            ""
            yield! content |> List.collect printContent
        ]
        String.Join (Dsl.NewLine, fileLines)

    let writeFile (repositoryPath: RepositoryPath) (content: Content list) =
        let fileContent = buildFileContent content
        let filePath = Dsl.getScanFileFilePath repositoryPath
        File.WriteAllText(filePath, fileContent)
        |> Ok

    let private removeComments (contentLine: string) =
        not (String.IsNullOrWhiteSpace contentLine || contentLine.StartsWith "#")

    let private parseContent (contentLine: string) =
        match contentLine.Split ' ' |> Seq.toList with
        | _::"(directory)"::path -> Ok (String.Join(' ', path).Replace("\"", ""))
        | _::"(file)"::path -> Ok (String.Join(' ', path).Replace("\"", ""))
        | _ -> Error "Invalid format"

    let readFile (repositoryPath: RepositoryPath) () =
        Dsl.getScanFileFilePath repositoryPath
        |> File.ReadAllLines
        |> Seq.filter removeComments
        |> Seq.map parseContent
        |> Seq.fold (fun result content ->
            match result, content with
            | Ok result, Ok content -> Ok (result@[content])
            | _, Error error
            | Error error, _ -> Error error
        ) (Ok [])

module TrackFile =
    let save (repositoryPath: RepositoryPath) (content: TrackedElement list) =
        let filePath = Dsl.getTrackFileFilePath repositoryPath
        File.WriteAllLines (filePath, content)
        Ok ()
