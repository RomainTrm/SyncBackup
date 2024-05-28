module SyncBackup.Commands.Config

open SyncBackup
open SyncBackup.Domain.Dsl

type Infra = {
    InitConfig: RepositoryConfig -> Result<unit, string>
    LoadConfig: unit -> Result<RepositoryConfig, string>
    CheckPathExists: DirectoryPath -> Result<unit, string>
    BuildRelativePath: Alias list -> UnverifiedPath -> Result<RelativePath, string>
    UpdateConfig: RepositoryConfig -> Result<unit, string>
    SolveRuleConflict: Rule -> Rule -> Result<Rule, string>
}

module Init =
    let source (infra: Infra) =
        let config = {
            Type = RepositoryType.Source
            Aliases = []
            Rules = []
        }
        infra.InitConfig config

    let backup (infra: Infra) =
        let config = {
            Type = RepositoryType.Backup
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
            | { Type = RepositoryType.Backup } -> Error "Aliases are only supported by source repositories"
            | { Aliases = aliases } when aliases |> List.contains alias -> Ok ()
            | { Aliases = aliases } when aliases |> List.exists (fun a -> a.Name = alias.Name) ->
                Error $"""The alias "{alias.Name}" already exists for another directory."""
            | config -> infra.UpdateConfig { config with Aliases = config.Aliases@[alias] }
        )

module Rules =
    open SyncBackup.Domain.Rules

    let add (infra: Infra) (rule: SyncRules) (unverifiedPath: UnverifiedPath) =
        result {
            let! config = infra.LoadConfig ()
            do! validateRule (config.Type, rule)
            let! path = infra.BuildRelativePath config.Aliases unverifiedPath
            let rule = { SyncRule = rule; Path = path }
            match add config.Rules rule with
            | Added rules -> return! infra.UpdateConfig { config with Rules = rules }
            | Conflict(rule1, rule2) ->
                return!
                    infra.SolveRuleConflict rule1 rule2
                    |> Result.bind (fun ruleToSave ->
                        let rules = replace config.Rules ruleToSave
                        infra.UpdateConfig { config with Rules = rules }
                    )
            | RuleAlreadyThere -> return ()
        }
