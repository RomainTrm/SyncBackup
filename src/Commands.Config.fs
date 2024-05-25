module SyncBackup.Commands.Config

open SyncBackup.Domain.Dsl

type Infra = {
    InitConfig: RepositoryConfig -> Result<unit, string>
    LoadConfig: unit -> Result<RepositoryConfig, string>
    CheckPathExists: DirectoryPath -> Result<unit, string>
    UpdateConfig: RepositoryConfig -> Result<unit, string>
    SolveRuleConflict: Rule -> Rule -> Result<Rule, string>
}

module Init =
    let run (infra: Infra) =
        let config = {
            IsSourceRepository = true
            Aliases = []
            Rules = []
        }
        infra.InitConfig config

module Alias =
    let private validateAliasName (name: string) =
        let forbiddenChars = "\\/:*?\"<>|" // TODO : use Path.GetInvalidPathChars instead
        if forbiddenChars |> Seq.exists name.Contains
        then Error $"Alias name contains forbidden characters ({forbiddenChars})"
        else Ok ()

    let add (infra: Infra) (alias: Alias) =
        validateAliasName alias.Name
        |> Result.bind (fun () -> infra.CheckPathExists alias.Path)
        |> Result.bind infra.LoadConfig
        |> Result.bind (function
            | { IsSourceRepository = false } -> Error "Aliases are only supported by source repositories"
            | { Aliases = aliases } when aliases |> List.contains alias -> Ok ()
            | { Aliases = aliases } when aliases |> List.exists (fun a -> a.Name = alias.Name) ->
                Error $"""The alias "{alias.Name}" already exists for another directory."""
            | config -> infra.UpdateConfig { config with Aliases = config.Aliases@[alias] }
        )

module Rules =
    let add (infra: Infra) (rule: Rule) =
        infra.LoadConfig ()
        |> Result.bind (function
            | { Rules = rules } when rules |> List.contains rule -> Ok ()
            | config when config.Rules |> List.exists (fun r -> r.Path = rule.Path) ->
                let existingRule = config.Rules |> List.find (fun r -> r.Path = rule.Path)
                infra.SolveRuleConflict existingRule rule
                |> Result.bind (fun ruleToSave ->
                    let rules =
                        config.Rules
                        |> List.except [existingRule]
                        |> List.append [ruleToSave]
                    infra.UpdateConfig { config with Rules = rules }
                )
            | config -> infra.UpdateConfig { config with Rules = config.Rules@[rule] }
        )
