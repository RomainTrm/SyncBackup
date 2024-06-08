module SyncBackup.Infra.Config

open System
open System.IO
open Microsoft.FSharp.Core
open SyncBackup
open SyncBackup.Domain.Dsl

module private FileSerializer =
    open Newtonsoft.Json

    let serialize (config: RepositoryConfig) = JsonConvert.SerializeObject(config, Formatting.Indented)
    let deserialize = JsonConvert.DeserializeObject<RepositoryConfig>

module private Init =
    let createConfigDirectory repositoryPath =
        let configPath = Path.Combine(repositoryPath, Dsl.ConfigDirectory)
        if (not<<Directory.Exists) configPath
        then Directory.CreateDirectory configPath |> ignore<DirectoryInfo>
        Ok ()

    let createConfigFile repositoryPath repositoryConfigContent =
        let filePath = Dsl.getFullConfigFilePath repositoryPath
        if File.Exists filePath
        then Error "A repository is already initialized here"
        else
            File.WriteAllText (filePath, repositoryConfigContent)
            |> Ok

let init (repositoryPath: RepositoryPath) (config: RepositoryConfig) =
    let fileContent = FileSerializer.serialize config

    Init.createConfigDirectory repositoryPath
    |> Result.bind (fun () -> Init.createConfigFile repositoryPath fileContent)

let private getConfigFilePath (repositoryPath: RepositoryPath) =
    let filePath = Dsl.getFullConfigFilePath repositoryPath
    if (not<<File.Exists) filePath
    then Error "No repository in the current directory"
    else Ok filePath

let update (repositoryPath: RepositoryPath) (config: RepositoryConfig) =
    getConfigFilePath repositoryPath
    |> Result.bind (fun configFilePath ->
        let fileContent = FileSerializer.serialize config
        File.WriteAllText (configFilePath, fileContent)
        Ok ()
    )

let load (repositoryPath: RepositoryPath) =
    getConfigFilePath repositoryPath
    |> Result.bind (File.ReadAllText >> FileSerializer.deserialize >> Ok)

module RuleEditionFile =
    let rec private print (rule: Rule) =
        $"{SyncRules.getValue rule.SyncRule} {RelativePath.serialize rule.Path}"

    let private buildFileContent repositoryType (rules: Rule list) =
        let fileLines = [
            $"# You can specify rules to every line (directories and files), by default '{SyncRules.getValue NoRule}' is set"
            "# Available rules as follows:"
            yield! SyncRules.getRulesAvailable repositoryType
                    |> Seq.map (SyncRules.getDescription >> sprintf "# - %s")
            ""
            yield! rules |> List.map print
            if rules = []
            then "# Your repository is empty, no rule to edit."
        ]
        String.Join (Dsl.NewLine, fileLines)

    let saveFile (repositoryPath: RepositoryPath) (repositoryType: RepositoryType) (rules: Rule list) =
        let fileContent = buildFileContent repositoryType rules
        let filePath = Dsl.getRulesEditionFilePath repositoryPath
        File.WriteAllText(filePath, fileContent)
        |> Ok

    let private removeComments (contentLine: string) =
        not (String.IsNullOrWhiteSpace contentLine || contentLine.StartsWith "#")

    let private parseRulesResult (contentLine: string) =
        match contentLine.Split ' ' |> Seq.toList with
        | []
        | [_] -> Error "Invalid format"
        | rule::path ->
            result {
                let! rule = SyncRules.parse rule
                let! path = String.Join(' ', path) |> RelativePath.deserialize
                return { SyncRule = rule; Path = path }
            }

    let loadFile (repositoryPath: RepositoryPath) =
        Dsl.getRulesEditionFilePath repositoryPath
        |> File.ReadAllLines
        |> Seq.filter removeComments
        |> Seq.map parseRulesResult
        |> Seq.fold (fun result content ->
            match result, content with
            | Ok result, Ok content -> Ok (content::result)
            | _, Error error
            | Error error, _ -> Error error
        ) (Ok [])
        |> Result.map List.rev
