module SyncBackup.Infra.Config

open System.IO
open SyncBackup.Domain.Dsl

let private configFilePath rootPath = Path.Combine(rootPath, Dsl.ConfigDirectory, Dsl.ConfigFile)

module private FileSerializer =
    let write (config: RepositoryConfig) =
        [|
            "[main]"
            "\tisSourceRepository = " + config.IsSourceRepository.ToString().ToLowerInvariant()
        |]

module private Init =
    let createConfigDirectory rootPath =
        let configPath = Path.Combine(rootPath, Dsl.ConfigDirectory)
        if (not<<Directory.Exists) configPath
        then Directory.CreateDirectory configPath |> ignore<DirectoryInfo>
        Ok ()

    let createConfigFile rootPath repositoryConfigContent =
        let filePath = configFilePath rootPath
        if File.Exists filePath
        then Error "A repository is already initialized here"
        else
            use stream = File.CreateText filePath
            stream.Close ()

            File.WriteAllLines (filePath, repositoryConfigContent)
            |> Ok


let init (path: FilePath) (config: RepositoryConfig) =
    let fileContent = FileSerializer.write config

    Init.createConfigDirectory path
    |> Result.bind (fun () -> Init.createConfigFile path fileContent)
