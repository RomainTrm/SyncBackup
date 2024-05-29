module SyncBackup.Tests.Domain.Sync

open SyncBackup.Domain.Dsl
open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Sync

module ``synchronize should`` =
    let d1 = { Type = Alias; Value = "d1"; ContentType = Directory }
    let d2 = { Type = Alias; Value = "d2"; ContentType = Directory }
    let d3 = { Type = Alias; Value = "d3"; ContentType = Directory }
    let d2f1 = { Type = Alias; Value = "d2/f1"; ContentType = File }
    let d2f2 = { Type = Alias; Value = "d2/f2"; ContentType = File }
    let d2f3 = { Type = Alias; Value = "d2/f3"; ContentType = File }
    let d3f1 = { Type = Alias; Value = "d3/f1"; ContentType = File }
    let d3f2 = { Type = Alias; Value = "d3/f2"; ContentType = File }
    let d3f3 = { Type = Alias; Value = "d3/f3"; ContentType = File }

    [<Fact>]
    let ``compute synchronize instructions when empty backup`` () =
        let sourceRules: Rule list = [
            { Path = d1; SyncRule = SyncRules.NoRule }
            { Path = d2; SyncRule = SyncRules.Include }
            { Path = d3; SyncRule = SyncRules.Exclude }
            { Path = d2f1; SyncRule = SyncRules.NoRule }
            { Path = d2f2; SyncRule = SyncRules.Include }
            { Path = d2f3; SyncRule = SyncRules.Exclude }
            { Path = d3f1; SyncRule = SyncRules.NoRule }
            { Path = d3f2; SyncRule = SyncRules.Include }
            { Path = d3f3; SyncRule = SyncRules.Exclude }
        ]

        let result = synchronize sourceRules []
        let expected = [
            Add d1
            Add d2
            Add d2f1
            Add d2f2
            Add d3f2
        ]
        test <@ result = expected @>

    [<Fact>]
    let ``compute synchronize instructions when not empty backup`` () =
        let sourceRules: Rule list = [
            { Path = d2; SyncRule = SyncRules.NoRule }
            { Path = d3; SyncRule = SyncRules.NoRule }
            { Path = d2f1; SyncRule = SyncRules.NoRule }
            { Path = d2f2; SyncRule = SyncRules.NoRule }
            { Path = d2f3; SyncRule = SyncRules.NoRule }
        ]

        let result = synchronize sourceRules [
            { Path = d1; SyncRule = SyncRules.NoRule }
            { Path = d2; SyncRule = SyncRules.NoRule }
            { Path = d3; SyncRule = SyncRules.NoRule }
            { Path = d2f1; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3; SyncRule = SyncRules.NotSave }
            { Path = d3f1; SyncRule = SyncRules.NotDelete }
            { Path = d3f2; SyncRule = SyncRules.NotDelete }
            { Path = d3f3; SyncRule = SyncRules.NotDelete }
        ]
        let expected = [
            Delete d1
            Replace d2f1
            Replace d2f2
        ]
        test <@ result = expected @>
