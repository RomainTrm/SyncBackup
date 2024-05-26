module SyncBackup.Domain.Scan

open SyncBackup.Domain.Dsl

let buildScanResult (existingRules: Rule list) (trackedElements: RelativePath list) (scannedElements: RelativePath list) =
    match scannedElements with
    | [] -> Error "Repository is empty."
    | _ ->
        scannedElements
        |> Rules.buildRules existingRules
        |> List.map (fun rule -> rule, Added)
        |> Ok

