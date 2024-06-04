module SyncBackup.Tests.Domain.Sync

open System
open SyncBackup.Domain.Dsl
open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Sync

module ``synchronize should`` =
    let d1 = { Type = Source; Value = "d1"; ContentType = Directory }
    let d2 = { Type = Source; Value = "d2"; ContentType = Directory }
    let d3 = { Type = Source; Value = "d3"; ContentType = Directory }
    let d2f1 = { Type = Source; Value = "d2/f1"; ContentType = File }
    let d2f2 = { Type = Source; Value = "d2/f2"; ContentType = File }
    let d2f3 = { Type = Source; Value = "d2/f3"; ContentType = File }
    let d2f4 = { Type = Source; Value = "d2/f4"; ContentType = File }
    let d3f1 = { Type = Source; Value = "d3/f1"; ContentType = File }
    let d3f2 = { Type = Source; Value = "d3/f2"; ContentType = File }
    let d3f3 = { Type = Source; Value = "d3/f3"; ContentType = File }

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
            Add d3
            Add d3f2
        ]
        test <@ result = Ok expected @>

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
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions when not empty backup (conflicting rules)`` () =
        let sourceRules: Rule list = [
            { Path = d1; SyncRule = SyncRules.Include }
            { Path = d2; SyncRule = SyncRules.Include }
            { Path = d3; SyncRule = SyncRules.NoRule }
            { Path = d2f1; SyncRule = SyncRules.Exclude }
            { Path = d2f2; SyncRule = SyncRules.Exclude }
            { Path = d2f3; SyncRule = SyncRules.Exclude }
            { Path = d2f4; SyncRule = SyncRules.Exclude }
        ]

        let result = synchronize sourceRules [
            { Path = d1; SyncRule = SyncRules.NotDelete }
            { Path = d2; SyncRule = SyncRules.NoRule }
            { Path = d3; SyncRule = SyncRules.NoRule }
            { Path = d2f1; SyncRule = SyncRules.NoRule }
            { Path = d2f2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3; SyncRule = SyncRules.NotDelete }
            { Path = d2f4; SyncRule = SyncRules.NotSave }
            { Path = d3f1; SyncRule = SyncRules.NotSave }
            { Path = d3f2; SyncRule = SyncRules.AlwaysReplace }
        ]

        let expected = [
            Delete d2f1
            Delete d2f2
            Delete d2f4
            Delete d3f1
            Delete d3f2
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions when source repository contains aliases`` () =
        let sourceRules: Rule list = [
            { Path = { d2 with Type = Alias }; SyncRule = SyncRules.NoRule }
            { Path = { d2f1 with Type = Alias }; SyncRule = SyncRules.NoRule }
            { Path = { d2f2 with Type = Alias }; SyncRule = SyncRules.NoRule }
            { Path = { d2f3 with Type = Alias }; SyncRule = SyncRules.NoRule }
        ]

        let result = synchronize sourceRules [
            { Path = d2; SyncRule = SyncRules.NoRule }
            { Path = d2f1; SyncRule = SyncRules.NotDelete }
            { Path = d2f2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3; SyncRule = SyncRules.AlwaysReplace }
        ]

        let expected = [
            Replace { d2f2 with Type = Alias }
            Replace { d2f3 with Type = Alias }
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions should only replace files`` () =
        let sourceRules: Rule list = [
            { Path = d1; SyncRule = SyncRules.NoRule }
            { Path = d2; SyncRule = SyncRules.NoRule }
            { Path = d3; SyncRule = SyncRules.NoRule }
            { Path = d2f1; SyncRule = SyncRules.NoRule }
            { Path = d2f2; SyncRule = SyncRules.NoRule }
            { Path = d2f3; SyncRule = SyncRules.NoRule }
        ]

        let result = synchronize sourceRules [
            { Path = d1; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d3; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f1; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3; SyncRule = SyncRules.AlwaysReplace }
        ]

        let expected = [
            Replace d2f1
            Replace d2f2
            Replace d2f3
        ]
        test <@ result = Ok expected @>

    let d1s1 = { Type = Source; Value = "d1/s1"; ContentType = Directory }
    let d1s2 = { Type = Source; Value = "d1/s2"; ContentType = Directory }
    let d1s1f1 = { Type = Source; Value = "d1/s1/f1"; ContentType = File }
    let d1s1f2 = { Type = Source; Value = "d1/s1/f2"; ContentType = File }
    let d1s2f1 = { Type = Source; Value = "d1/s2/f1"; ContentType = File }

    let rnd = Random();
    let randomizeOrder _ _ = rnd.Next ()

    [<Fact>]
    let ``compute add order`` () =
        let sourceRules = List.sortWith randomizeOrder [
            { Path = d1; SyncRule = SyncRules.NoRule }
            { Path = d1s1; SyncRule = SyncRules.NoRule }
            { Path = d1s1f1; SyncRule = SyncRules.NoRule }
            { Path = d1s1f2; SyncRule = SyncRules.NoRule }
            { Path = d1s2; SyncRule = SyncRules.NoRule }
            { Path = d1s2f1; SyncRule = SyncRules.NoRule }
            { Path = d2; SyncRule = SyncRules.NoRule }
            { Path = d2f1; SyncRule = SyncRules.NoRule }
            { Path = d2f2; SyncRule = SyncRules.NoRule }
        ]

        let result = synchronize sourceRules []

        let expected = [
            Add d1
            Add d1s1
            Add d1s1f1
            Add d1s1f2
            Add d1s2
            Add d1s2f1
            Add d2
            Add d2f1
            Add d2f2
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute delete order`` () =
        let backupRules = List.sortWith randomizeOrder [
            { Path = d1; SyncRule = SyncRules.NoRule }
            { Path = d1s1; SyncRule = SyncRules.NoRule }
            { Path = d1s1f1; SyncRule = SyncRules.NoRule }
            { Path = d1s1f2; SyncRule = SyncRules.NoRule }
            { Path = d1s2; SyncRule = SyncRules.NoRule }
            { Path = d1s2f1; SyncRule = SyncRules.NoRule }
            { Path = d2; SyncRule = SyncRules.NoRule }
            { Path = d2f1; SyncRule = SyncRules.NoRule }
            { Path = d2f2; SyncRule = SyncRules.NoRule }
        ]

        let result = synchronize [] backupRules

        let expected = [
            Delete d1s1f1
            Delete d1s1f2
            Delete d1s1
            Delete d1s2f1
            Delete d1s2
            Delete d1
            Delete d2f1
            Delete d2f2
            Delete d2
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``not delete directory if keeping children`` () =
        let backupRules = [
            { Path = d1; SyncRule = SyncRules.NoRule }
            { Path = d1s1; SyncRule = SyncRules.NoRule }
            { Path = d1s1f1; SyncRule = SyncRules.NoRule }
            { Path = d1s1f2; SyncRule = SyncRules.NotDelete }
            { Path = d1s2; SyncRule = SyncRules.NoRule }
            { Path = d1s2f1; SyncRule = SyncRules.NoRule }
            { Path = d2; SyncRule = SyncRules.NotDelete }
            { Path = d2f1; SyncRule = SyncRules.NoRule }
            { Path = d2f2; SyncRule = SyncRules.NoRule }
        ]

        let result = synchronize [] backupRules

        let expected = [
            Delete d1s1f1
            Delete d1s2f1
            Delete d1s2
        ]
        test <@ result = Ok expected @>
