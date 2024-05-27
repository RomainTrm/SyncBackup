module SyncBackup.Domain.Scan

open SyncBackup.Domain.Dsl

let buildScanResult (existingRules: Rule list) (trackedElements: RelativePath list) (scannedElements: RelativePath list) =
    match scannedElements with
    | [] -> Error "Repository is empty."
    | _ ->
        let trackedElements = Set trackedElements
        let scannedElements = Set scannedElements

        let added =
            Set.difference scannedElements trackedElements
            |> Set.toList
            |> Rules.buildRules existingRules
            |> List.map (ScanResult.build AddedToRepository)

        let removed =
            Set.difference trackedElements scannedElements
            |> Set.toList
            |> Rules.buildRules existingRules
            |> List.map (ScanResult.build RemovedFromRepository)

        let delta = added@removed
        let rulesReminders =
            existingRules
            |> List.collect (fun existingRule ->
                let deltaPaths = delta |> Seq.map _.Path
                match deltaPaths |> Seq.contains existingRule.Path with
                | true -> []
                | false ->
                    if deltaPaths |> Seq.exists (fun childPath -> RelativePath.contains childPath existingRule.Path)
                    then [ScanResult.build RuleReminder existingRule]
                    else []
            )

        (delta@rulesReminders)
        |> List.sortBy _.Path.Value
        |> Ok

let defineTrackedElements (trackedElements: RelativePath list) (scannedElements: ScanResult list) =
    let filter diff = scannedElements |> Seq.filter (fun scan -> scan.Diff = diff) |> Seq.map _.Path |> Set
    let removedElements = filter RemovedFromRepository
    let addedElements = filter AddedToRepository

    Set.difference (Set trackedElements) removedElements
    |> Set.union addedElements
    |> Set.toList
