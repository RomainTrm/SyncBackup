module SyncBackup.Commands.Config

open SyncBackup.Domain.Dsl

type Infra = {
    InitConfig: RepositoryPath -> RepositoryConfig -> Result<unit, string>
    LoadConfig: RepositoryPath -> Result<RepositoryConfig, string>
    CheckPathExists: DirectoryPath -> Result<unit, string>
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
    let private validateAliasName (name: string) =
        let forbiddenChars = "\\/:*?\"<>|"
        if forbiddenChars |> Seq.exists name.Contains
        then Error $"Alias name contains forbidden characters ({forbiddenChars})"
        else Ok ()

    let add (infra: Infra) (repositoryPath: RepositoryPath) (alias: Alias) =
        validateAliasName alias.Name
        |> Result.bind (fun () -> infra.CheckPathExists alias.Path)
        |> Result.bind (fun () -> infra.LoadConfig repositoryPath)
        |> Result.bind (fun config ->
            if config.Aliases |> List.contains alias
            then Ok ()
            elif config.Aliases |> List.exists (fun a -> a.Name = alias.Name)
            then Error $"""The alias "{alias.Name}" already exists for another directory."""
            else
                let config = { config with Aliases = config.Aliases@[alias] }
                infra.UpdateConfig repositoryPath config
        )
