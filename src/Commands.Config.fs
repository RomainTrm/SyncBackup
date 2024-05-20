module SyncBackup.Commands.Config

open SyncBackup.Domain.Dsl

type Infra = {
    InitConfig: RepositoryPath -> RepositoryConfig -> Result<unit, string>
    LoadConfig: RepositoryPath -> Result<RepositoryConfig, string>
    UpdateConfig: RepositoryPath -> RepositoryConfig -> Result<unit, string>
}

module Init =
    let run (infra: Infra) (repositoryPath: RepositoryPath) =
        let config = {
            IsMainRepository = true
            Aliases = []
        }
        infra.InitConfig repositoryPath config

module Alias =
    let add (infra: Infra) (repositoryPath: RepositoryPath) (alias: Alias) =
        infra.LoadConfig repositoryPath
        |> Result.map (fun config ->
            { config with Aliases = config.Aliases@[alias] }
        )
        |> Result.bind (infra.UpdateConfig repositoryPath)
