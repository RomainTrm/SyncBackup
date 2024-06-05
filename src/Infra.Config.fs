﻿module SyncBackup.Infra.Config

open System.IO
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
