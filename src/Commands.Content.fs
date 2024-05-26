module SyncBackup.Commands.Content

open Microsoft.FSharp.Core
open SyncBackup.Domain.Dsl
open SyncBackup.Domain

type Infra = {
    LoadConfig: unit -> Result<RepositoryConfig, string>
    LoadRepositoryContent: Alias list -> RelativePath list
    SaveTempContent: Rule list -> Result<unit, string>
    OpenForUserEdition: unit -> Result<unit, string>
    ReadTempContent: unit -> Result<Rule list, string>
    SaveTrackFile: RelativePath list -> Result<unit, string>
    SaveRules: Rule list -> Result<unit, string>
}

let private updateRules oldRules =
    List.fold (fun rules rule ->
        match Rules.add rules rule with
        | Rules.Added rules -> rules
        | Rules.RuleAlreadyThere -> rules
        | Rules.Conflict _ ->
            // users chose to override value while editing the file, so we can safely override old rule
            Rules.replace rules rule
    ) oldRules

let rec private buildRule (existingRules: Map<RelativePath, Rule>) path =
    existingRules
    |> Map.tryFind path
    |> Option.defaultValue { Path = path; SyncRule = NoRule }

let scanRepositoryContent (infra: Infra) () =
    SyncBackup.Helpers.result {
        let! config = infra.LoadConfig ()
        let existingRules = config.Rules |> Seq.map (fun rule -> rule.Path, rule) |> Map
        let! repositoryContent =
            config.Aliases
            |> infra.LoadRepositoryContent
            |> function
                | [] -> Error "Repository is empty."
                | content -> content |> List.map (buildRule existingRules) |> Ok

        do! infra.SaveTempContent repositoryContent
        do! infra.OpenForUserEdition ()

        let! editedRules = infra.ReadTempContent ()
        do! infra.SaveTrackFile (editedRules |> List.map _.Path)
        let rulesToSave = editedRules |> updateRules config.Rules
        do! infra.SaveRules rulesToSave

        return ()
    }
