module SyncBackup.Tests.Domain.Sync

open System
open SyncBackup.Domain.Dsl
open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Sync

let d1 = { Type = Source; Value = "d1"; ContentType = Directory }
let d2 = { Type = Source; Value = "d2"; ContentType = Directory }
let d3 = { Type = Source; Value = "d3"; ContentType = Directory }
let d1s1 = { Type = Source; Value = "d1/s1"; ContentType = Directory }
let d1s2 = { Type = Source; Value = "d1/s2"; ContentType = Directory }
let d1s1f1 = { Type = Source; Value = "d1/s1/f1"; ContentType = File }
let d1s1f2 = { Type = Source; Value = "d1/s1/f2"; ContentType = File }
let d1s2f1 = { Type = Source; Value = "d1/s2/f1"; ContentType = File }
let d2f1 = { Type = Source; Value = "d2/f1"; ContentType = File }
let d2f2 = { Type = Source; Value = "d2/f2"; ContentType = File }
let d2f3 = { Type = Source; Value = "d2/f3"; ContentType = File }
let d2f4 = { Type = Source; Value = "d2/f4"; ContentType = File }
let d3f1 = { Type = Source; Value = "d3/f1"; ContentType = File }
let d3f2 = { Type = Source; Value = "d3/f2"; ContentType = File }
let d3f3 = { Type = Source; Value = "d3/f3"; ContentType = File }

let rnd = Random();
let randomizeOrder _ _ = rnd.Next ()

module ``synchronize should`` =
    [<Fact>]
    let ``compute synchronize instructions when empty backup`` () =
        let sourceItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
            d3f1
            d3f2
            d3f3
        ]

        let sourceRules: Rule list = [
            { Path = d2; SyncRule = SyncRules.Include }
            { Path = d3; SyncRule = SyncRules.Exclude }
            { Path = d2f2; SyncRule = SyncRules.Include }
            { Path = d2f3; SyncRule = SyncRules.Exclude }
            { Path = d3f2; SyncRule = SyncRules.Include }
            { Path = d3f3; SyncRule = SyncRules.Exclude }
        ]

        let result = Synchronize.run sourceItems sourceRules [] []

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
        let sourceItems = [
            d2
            d3
            d2f1
            d2f2
            d2f3
        ]

        let backupItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
            d3f1
            d3f2
            d3f3
        ]

        let backupRules = [
            { Path = d2f1; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3; SyncRule = SyncRules.NotSave }
            { Path = d3f1; SyncRule = SyncRules.NotDelete }
            { Path = d3f2; SyncRule = SyncRules.NotDelete }
            { Path = d3f3; SyncRule = SyncRules.NotDelete }
        ]

        let result = Synchronize.run sourceItems [] backupItems backupRules

        let expected = [
            Delete d1
            Replace d2f1
            Replace d2f2
            Delete d2f3
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions when not empty backup (conflicting rules)`` () =
        let sourceItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
            d2f4
        ]

        let sourceRules: Rule list = [
            { Path = d1; SyncRule = SyncRules.Include }
            { Path = d2; SyncRule = SyncRules.Include }
            { Path = d2f1; SyncRule = SyncRules.Exclude }
            { Path = d2f2; SyncRule = SyncRules.Exclude }
            { Path = d2f3; SyncRule = SyncRules.Exclude }
            { Path = d2f4; SyncRule = SyncRules.Exclude }
        ]

        let backupItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
            d2f4
            d3f1
            d3f2
        ]

        let backupRules = [
            { Path = d1; SyncRule = SyncRules.NotDelete }
            { Path = d2f2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3; SyncRule = SyncRules.NotDelete }
            { Path = d2f4; SyncRule = SyncRules.NotSave }
            { Path = d3f1; SyncRule = SyncRules.NotSave }
            { Path = d3f2; SyncRule = SyncRules.AlwaysReplace }
        ]

        let result = Synchronize.run sourceItems sourceRules backupItems backupRules

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
        let sourceItems = [
            { d2 with Type = Alias }
            { d2f1 with Type = Alias }
            { d2f2 with Type = Alias }
            { d2f3 with Type = Alias }
        ]

        let backupItems = [
            d2
            d2f1
            d2f2
            d2f3
        ]

        let backupRules = [
            { Path = d2f1; SyncRule = SyncRules.NotDelete }
            { Path = d2f2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3; SyncRule = SyncRules.AlwaysReplace }
        ]

        let result = Synchronize.run sourceItems [] backupItems backupRules

        let expected = [
            Replace { d2f2 with Type = Alias }
            Replace { d2f3 with Type = Alias }
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions should only replace files`` () =
        let sourceItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
        ]

        let backupItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
        ]

        let backupRules = [
            { Path = d1; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d3; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f1; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3; SyncRule = SyncRules.AlwaysReplace }
        ]

        let result = Synchronize.run sourceItems [] backupItems backupRules

        let expected = [
            Replace d2f1
            Replace d2f2
            Replace d2f3
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute add order`` () =
        let sourceItems = List.sortWith randomizeOrder [
            d1
            d1s1
            d1s1f1
            d1s1f2
            d1s2
            d1s2f1
            d2
            d2f1
            d2f2
        ]

        let result = Synchronize.run sourceItems [] [] []

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
        let backupItems = List.sortWith randomizeOrder [
            d1
            d1s1
            d1s1f1
            d1s1f2
            d1s2
            d1s2f1
            d2
            d2f1
            d2f2
        ]

        let result = Synchronize.run [] [] backupItems []

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
        let backupItems = [
            d1
            d1s1
            d1s1f1
            d1s1f2
            d1s2
            d1s2f1
            d2
            d2f1
            d2f2
        ]

        let backupRules = [
            { Path = d1s1f2; SyncRule = SyncRules.NotDelete }
            { Path = d2; SyncRule = SyncRules.NotDelete }
        ]

        let result = Synchronize.run [] [] backupItems backupRules

        let expected = [
            Delete d1s1f1
            Delete d1s2f1
            Delete d1s2
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``not delete element even if excluded`` () =
        let sourceItems = [
            d2
            d2f1
        ]

        let sourceRules = [
            { Path = d2f1; SyncRule = SyncRules.Exclude }
        ]

        let backupItems = [
            d2
            d2f1
        ]

        let backupRules = [
            { Path = d2f1; SyncRule = SyncRules.NotDelete }
        ]

        let result = Synchronize.run sourceItems sourceRules backupItems backupRules

        test <@ result = Ok [] @>

    [<Fact>]
    let ``apply 'ignore' rule to children`` () =
        let sourceItems = [
            d1
            d2
            d2f1
            d2f2
            d3
            d3f1
            d3f2
        ]

        let sourceRules = [
            { Path = d3f1; SyncRule = SyncRules.Exclude }
            { Path = d3f2; SyncRule = SyncRules.Include }
        ]

        let backupRules = List.sortWith randomizeOrder [
            { Path = d1; SyncRule = SyncRules.NotSave }
            { Path = d2; SyncRule = SyncRules.NotSave }
            { Path = d3; SyncRule = SyncRules.NotSave }
        ]

        let result = Synchronize.run sourceItems sourceRules [] backupRules

        test <@ result = Ok [] @>

    [<Fact>]
    let ``not delete root directory when excluded but another rule for a file inside override it`` () =
        let sourceItems = [
            d3
            d3f1
            d3f2
            d3f3
        ]

        let backupItems = [
            d3
            d3f1
        ]

        let backupRules = List.sortWith randomizeOrder [
            { Path = d3; SyncRule = SyncRules.NotSave }
            { Path = d3f1; SyncRule = SyncRules.AlwaysReplace }
        ]

        let result = Synchronize.run sourceItems [] backupItems backupRules

        test <@ result = Ok [
            Replace d3f1
        ] @>

module ``replicate should`` =
    [<Fact>]
    let ``compute synchronize instructions when empty backup`` () =
        let sourceItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
            d3f1
            d3f2
            d3f3
        ]

        let result = Replicate.run [] sourceItems []

        let expected = [
            Add d1
            Add d2
            Add d2f1
            Add d2f2
            Add d2f3
            Add d3
            Add d3f1
            Add d3f2
            Add d3f3
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions when not empty backup`` () =
        let sourceItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
            d3f1
            d3f2
            d3f3
        ]

        let backupItems = [
            d1
            d3
            d3f1
            d3f2
            d3f3
        ]

        let rules = [
            { Path = d3f1; SyncRule = SyncRules.AlwaysReplace }
            { Path = d3f2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d3f3; SyncRule = SyncRules.AlwaysReplace }
        ]

        let result = Replicate.run rules sourceItems backupItems

        let expected = [
            Add d2
            Add d2f1
            Add d2f2
            Add d2f3
            Replace d3f1
            Replace d3f2
            Replace d3f3
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions should only replace files`` () =
        let sourceItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
        ]

        let backupItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
        ]

        let rules = [
            { Path = d1; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d3; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f1; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f2; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3; SyncRule = SyncRules.AlwaysReplace }
        ]

        let result = Replicate.run rules sourceItems backupItems

        let expected = [
            Replace d2f1
            Replace d2f2
            Replace d2f3
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute add order`` () =
        let sourceItems = List.sortWith randomizeOrder [
            d1
            d1s1
            d1s1f1
            d1s1f2
            d1s2
            d1s2f1
            d2
            d2f1
            d2f2
        ]

        let result = Replicate.run [] sourceItems []

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
        let backupItems = List.sortWith randomizeOrder [
            d1
            d1s1
            d1s1f1
            d1s1f2
            d1s2
            d1s2f1
            d2
            d2f1
            d2f2
        ]

        let rules = [
            { Path = d2f1; SyncRule = NotSave }
            { Path = d2f2; SyncRule = AlwaysReplace }
        ]

        let result = Replicate.run rules [] backupItems

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
    let ``apply 'preserve' rule`` () =
        let backupItems = [
            d2
            d2f1
            d2f2
        ]

        let rules = [
            { Path = d2; SyncRule = NotDelete }
        ]
        let result = Replicate.run rules [] backupItems

        test <@ result = Ok [] @>
