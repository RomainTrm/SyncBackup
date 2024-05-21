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
            IsSourceRepository = true
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
        |> Result.bind (function
            | { IsSourceRepository = false } -> Error "Aliases are only supported by source repositories"
            | { Aliases = aliases } when aliases |> List.contains alias -> Ok ()
            | { Aliases = aliases } when aliases |> List.exists (fun a -> a.Name = alias.Name) ->
                Error $"""The alias "{alias.Name}" already exists for another directory."""
            | config ->
                let config = { config with Aliases = config.Aliases@[alias] }
                infra.UpdateConfig repositoryPath config
        )
