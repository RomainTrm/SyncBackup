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
        |> Result.bind (fun config ->
            if config.Aliases |> List.contains alias
            then Ok ()
            elif config.Aliases |> List.exists (fun a -> a.Name = alias.Name)
            then Error $"""The alias "{alias.Name}" already exists for another directory."""
            else
                let config = { config with Aliases = config.Aliases@[alias] }
                infra.UpdateConfig repositoryPath config
        )
