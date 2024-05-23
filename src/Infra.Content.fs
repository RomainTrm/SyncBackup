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
        let aliasesDirectoriesContent = List.collect (fun alias -> scan' alias.Path (aliasRelativePath alias)) aliases
        sourceDirectoryContent@aliasesDirectoriesContent

module ScanFile =
    open Microsoft.FSharp.Reflection

    let rec private printContent = function
        | Directory { RelativePath = Source path; Content = content }
        | Directory { RelativePath = Alias path; Content = content } ->
            [
                $"{SyncRules.getValue NoRule} (directory) \"{path}\""
                yield! content |> List.collect printContent
            ]
        | File { RelativePath = Source path }
        | File { RelativePath = Alias path } -> [ $"{SyncRules.getValue NoRule} (file) \"{path}\"" ]

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
