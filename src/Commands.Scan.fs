﻿module SyncBackup.Commands.Scan

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
    ResetScan: unit -> Result<unit, string>
}

let private updateRules repositoryType oldRules =
    List.fold (fun rules (rule: ScanResult) ->
        result {
            let! rules = rules
            do! Rules.validateRule repositoryType rule.Rule.SyncRule
            return rule.Rule::rules
        }
    ) (Ok [])
    >> Result.map List.rev
    >> Result.map (Rules.updateRulesAfterEdition oldRules)

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
        let! rulesToSave = updateRules config.Type config.Rules editedRules
        do! infra.SaveTrackFile (Scan.defineTrackedElements trackedElements editedRules)
        do! infra.SaveRules rulesToSave

        return ()
    }

let reset (infra: Infra) = infra.LoadConfig >> Result.bind (ignore<RepositoryConfig> >> infra.ResetScan)
