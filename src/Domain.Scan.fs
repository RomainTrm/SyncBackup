module SyncBackup.Domain.Scan

open SyncBackup.Domain.Dsl

let buildScanResult (existingRules: Rule list) (trackedElements: Content list) (scannedElements: Content list) =
    match scannedElements with
    | [] -> Error "Repository is empty."
    | _ ->
        let trackedElementsPaths = Set (trackedElements |> List.map _.Path)
        let scannedElementsPaths = Set (scannedElements |> List.map _.Path)
        let trackedElements = trackedElements |> Seq.map (fun content -> content.Path, content) |> Map.ofSeq
        let scannedElements = scannedElements |> Seq.map (fun content -> content.Path, content) |> Map.ofSeq

        let added =
            Set.difference scannedElementsPaths trackedElementsPaths
            |> Set.toList
            |> Rules.buildRulesForScanning existingRules
            |> List.map (ScanResult.build AddedToRepository)

        let removed =
            Set.difference trackedElementsPaths scannedElementsPaths
            |> Set.toList
            |> Rules.buildRulesForScanning existingRules
            |> List.map (ScanResult.build RemovedFromRepository)

        let updated =
            Set.intersect trackedElementsPaths scannedElementsPaths
            |> Set.toSeq
            |> Seq.choose (fun path ->
                match Map.tryFind path trackedElements, Map.tryFind path scannedElements with
                | Some { LastWriteTime = Some trackedTime }, Some { LastWriteTime = Some scanTime } when trackedTime < scanTime -> Some path
                | _ -> None
            )
            |> Seq.toList
            |> Rules.buildRulesForScanning existingRules
            |> List.map (ScanResult.build Updated)

        let delta = added@updated@removed
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
