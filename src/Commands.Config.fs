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
    SolveContentType: unit -> Result<ContentType, string>
    LoadTrackFile: unit -> Result<RelativePath list, string>
    SaveRulesFile: RepositoryType -> Rule list -> Result<unit, string>
    OpenRulesFile: unit -> Result<unit, string>
    ReadRulesFile: unit -> Result<Rule list, string>
}

module Init =
    let source (infra: Infra) =
        let config = {
            Version = RepositoryConfigVersion
            Type = RepositoryType.Source
            Aliases = []
            Rules = []
        }
        infra.InitConfig config

    let backup (infra: Infra) =
        let config = {
            Version = RepositoryConfigVersion
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
            do! validateRule config.Type rule
            let! path =
                match rule with
                | SyncRules.NotSave ->
                    infra.SolveContentType ()
                    |> Result.map (fun contentType ->
                        let pathType = Source // NotSave rule is only for backup repositories, so it's a Source
                        { Value = unverifiedPath; Type = pathType; ContentType = contentType }
                    )
                | _ -> infra.BuildRelativePath config.Aliases unverifiedPath

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

    let editRules (infra: Infra) () =
        result {
            let! config = infra.LoadConfig ()
            let! trackedElements = infra.LoadTrackFile ()
            let! repositoryContent = buildTreeWithRules config.Rules trackedElements

            do! infra.SaveRulesFile config.Type repositoryContent
            do! infra.OpenRulesFile ()

            let! editedRules = infra.ReadRulesFile ()
            let rulesToSave = updateRulesAfterEdition config.Rules editedRules
            do! infra.UpdateConfig { config with Rules = rulesToSave }

            return ()
        }
