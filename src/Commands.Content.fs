module SyncBackup.Commands.Content

open Microsoft.FSharp.Core
open SyncBackup
open SyncBackup.Domain
open SyncBackup.Domain.Dsl

type Infra = {
    LoadConfig: unit -> Result<RepositoryConfig, string>
    ScanRepositoryContent: Alias list -> RelativePath list
    SaveScanFileContent: ScanResult list -> Result<unit, string>
    OpenScanFileForUserEdition: unit -> Result<unit, string>
    ReadScanFileContent: unit -> Result<ScanResult list, string>
    SaveTrackFile: RelativePath list -> Result<unit, string>
    LoadTrackFile: unit -> Result<RelativePath list, string>
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

let scanRepositoryContent (infra: Infra) () =
    result {
        let! config = infra.LoadConfig ()
        let! trackedElements = infra.LoadTrackFile ()
        let! repositoryContent =
            config.Aliases
            |> infra.ScanRepositoryContent
            |> Scan.buildScanResult config.Rules trackedElements

        do! infra.SaveScanFileContent repositoryContent
        do! infra.OpenScanFileForUserEdition ()

        let! editedRules = infra.ReadScanFileContent ()
        do! infra.SaveTrackFile (Scan.defineTrackedElements trackedElements editedRules)
        let rulesToSave = editedRules |> List.map _.Rule |> updateRules config.Rules
        do! infra.SaveRules rulesToSave

        return ()
    }
