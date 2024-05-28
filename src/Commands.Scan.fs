module SyncBackup.Commands.Scan

open Microsoft.FSharp.Core
open SyncBackup
open SyncBackup.Domain
open SyncBackup.Domain.Dsl

type Infra = {
    LoadConfig: unit -> Result<RepositoryConfig, string>
    ScanRepositoryContent: Alias list -> RelativePath list
    SaveScanFileContent: RepositoryType -> ScanResult list -> Result<unit, string>
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

        do! infra.SaveScanFileContent config.Type repositoryContent
        do! infra.OpenScanFileForUserEdition ()

        let! editedRules = infra.ReadScanFileContent ()
        let! rulesToSave =
            editedRules
            |> List.map _.Rule
            |> List.fold (fun rules rule ->
                result {
                    do! Rules.validateRule config.Type rule.SyncRule
                    let! rules = rules
                    return rules@[rule]
                }
            ) (Ok [])
            |> Result.map (updateRules config.Rules)
        do! infra.SaveTrackFile (Scan.defineTrackedElements trackedElements editedRules)
        do! infra.SaveRules rulesToSave

        return ()
    }
