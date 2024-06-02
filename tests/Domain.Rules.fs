module SyncBackup.Tests.Domain.Rules

open Xunit
open Swensen.Unquote

open SyncBackup.Domain.Dsl
open SyncBackup.Domain.Rules

module ``buildRulesForSyncing should`` =
    let p1 = { Value = "p1"; Type = Source; ContentType = File }
    let p2 = { Value = "p2"; Type = Source; ContentType = File }
    let p3 = { Value = "p3"; Type = Source; ContentType = File }
    let p4 = { Value = "p4"; Type = Source; ContentType = File }

    [<Fact>]
    let ``apply rules correctly`` () =
        let existingElements = [p1; p2; p3; p4]
        let setupRules = [
            {  Path = p2; SyncRule = SyncRules.NotSave }
            {  Path = p3; SyncRule = SyncRules.Exclude }
            {  Path = p4; SyncRule = SyncRules.NotDelete }
        ]

        let result = buildRulesForSyncing setupRules existingElements

        let expected = [
            { Path = p1; SyncRule = SyncRules.NoRule }
            { Path = p2; SyncRule = SyncRules.NotSave }
            { Path = p3; SyncRule = SyncRules.Exclude }
            { Path = p4; SyncRule = SyncRules.NotDelete }
        ]
        test <@ result = expected @>
