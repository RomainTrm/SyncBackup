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
            |> List.map (fun rule -> rule, AddedToRepository)

        let removed =
            Set.difference trackedElements scannedElements
            |> Set.toList
            |> Rules.buildRules existingRules
            |> List.map (fun rule -> rule, RemovedFromRepository)

        let delta = added@removed
        let rulesReminders =
            existingRules
            |> List.collect (fun existingRule ->
                let deltaPaths = delta |> Seq.map (fst>>_.Path)
                match deltaPaths |> Seq.contains existingRule.Path with
                | true -> []
                | false ->
                    if deltaPaths |> Seq.exists (fun childPath -> RelativePath.contains childPath existingRule.Path)
                    then [existingRule, RuleReminder]
                    else []
            )

        (delta@rulesReminders)
        |> List.sortBy (fun (rule, _) -> rule.Path.Value)
        |> Ok

let defineTrackedElements (trackedElements: RelativePath list) (scannedElements: ScanResult list) =
    let removedElements = scannedElements |> Seq.filter (fun scan -> (snd scan) = RemovedFromRepository) |> Seq.map (fst>>_.Path) |> Set
    let addedElements = scannedElements |> Seq.filter (fun scan -> (snd scan) = AddedToRepository) |> Seq.map (fst>>_.Path) |> Set

    Set.difference (Set trackedElements) removedElements
    |> Set.union addedElements
    |> Set.toList
