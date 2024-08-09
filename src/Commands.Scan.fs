module SyncBackup.Commands.Scan

open Microsoft.FSharp.Core
open SyncBackup
open SyncBackup.Domain
open SyncBackup.Domain.Dsl

type Infra = {
    LoadConfig: unit -> Result<RepositoryConfig, string>
    ScanRepositoryContent: Alias list -> Content list
    SaveScanFileContent: RepositoryType -> ScanResult list -> Result<unit, string>
    OpenScanFileForUserEdition: unit -> Result<unit, string>
    ReadScanFileContent: unit -> Result<ScanResult list, string>
    SaveTrackFile: Content list -> Result<unit, string>
    LoadTrackFile: unit -> Result<Content list, string>
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

let private applyLastWriteTime (repositoryContent: Content list) (trackedElements: RelativePath list) =
    let repositoryContent = repositoryContent |> Seq.map (fun x -> x.Path, x) |> Map
    trackedElements |> List.map (fun x -> repositoryContent[x])

let scanRepositoryContent (infra: Infra) () =
    result {
        let! config = infra.LoadConfig ()
        let! trackedElements = infra.LoadTrackFile ()
        let trackedElementsPaths = trackedElements |> List.map _.Path
        let repositoryContent =
            config.Aliases
            |> infra.ScanRepositoryContent

        let! scanResult =
            repositoryContent
            |> List.map _.Path
            |> Scan.buildScanResult config.Rules trackedElementsPaths

        do! infra.SaveScanFileContent config.Type scanResult
        do! infra.OpenScanFileForUserEdition ()

        let! editedRules = infra.ReadScanFileContent ()
        let! rulesToSave = updateRules config.Type config.Rules editedRules
        let contentToTrack = Scan.defineTrackedElements trackedElementsPaths editedRules |> applyLastWriteTime repositoryContent
        do! infra.SaveTrackFile contentToTrack
        do! infra.SaveRules rulesToSave

        return ()
    }

let reset (infra: Infra) = infra.LoadConfig >> Result.bind (ignore<RepositoryConfig> >> infra.ResetScan)
