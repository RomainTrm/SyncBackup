module SyncBackup.Commands.Content

open Microsoft.FSharp.Core
open SyncBackup.Domain.Dsl
open SyncBackup.Domain

type Infra = {
    LoadConfig: unit -> Result<RepositoryConfig, string>
    LoadFiles: Alias list -> Content list
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

let rec private buildRule (existingRules: Map<RelativePath, Rule>) content =
    let getRule path =
        existingRules
        |> Map.tryFind path
        |> Option.defaultValue { Path = path; SyncRule = NoRule }

    match content with
    | File file -> [ getRule file.RelativePath ]
    | Directory directory -> [
        getRule directory.RelativePath
        yield! directory.Content |> List.collect (buildRule existingRules)
    ]

let scanRepositoryContent (infra: Infra) () =
    SyncBackup.Helpers.result {
        let! config = infra.LoadConfig ()
        let existingRules = config.Rules |> Seq.map (fun rule -> rule.Path, rule) |> Map
        let! repositoryContent =
            config.Aliases
            |> infra.LoadFiles
            |> function
                | [] -> Error "Repository is empty."
                | content -> content |> List.collect (buildRule existingRules) |> Ok

        do! infra.SaveTempContent repositoryContent
        do! infra.OpenForUserEdition ()

        let! editedRules = infra.ReadTempContent ()
        do! infra.SaveTrackFile (editedRules |> List.map _.Path)
        let rulesToSave = editedRules |> updateRules config.Rules
        do! infra.SaveRules rulesToSave

        return ()
    }
