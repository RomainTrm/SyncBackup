module SyncBackup.Tests.Domain.Scan

open SyncBackup.Tests.Properties.CustomGenerators
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Domain.Scan

module ``buildScanResult should`` =
    let isAsExpected expected result =
        let result = Result.map Set result
        let expected = Set expected
        test <@ result = Ok expected @>

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``return error when no element scanned`` rules tracked =
        let result = buildScanResult rules tracked []
        test <@ result = Error "Repository is empty." @>

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``return all paths as added without rule when no tracked neither rules`` paths =
        paths <> [] ==> lazy
        let result = buildScanResult [] [] paths
        let expected = paths |> List.map (fun path -> { SyncRule = NoRule; Path = path }, AddedToRepository)
        result |> isAsExpected expected

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``return path with existing rule`` path syncRule =
        let rule = { SyncRule = syncRule; Path = path }
        let result = buildScanResult [rule] [] [path]
        result |> isAsExpected [rule, AddedToRepository]

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``return only delta between scan and tracked elements`` (paths: RelativePath list) =
        let paths = List.distinct paths
        List.length paths > 3 ==> lazy
        let added::removed::common = paths
        let result = buildScanResult [] (removed::common) (added::common)
        let expected = [
            { SyncRule = NoRule; Path = removed }, RemovedFromRepository
            { SyncRule = NoRule; Path = added }, AddedToRepository
        ]
        result |> isAsExpected expected

    [<Fact>]
    let ``include rule reminder for the user when one parent has rule but remains unchanged`` () =
        let subPath = { Value = "dir"; Type = Source; ContentType = ContentType.Directory }
        let path = { Value = "dir\\file"; Type = Source; ContentType = ContentType.File }

        let result = buildScanResult [{ SyncRule = Include; Path = subPath }] [subPath] [subPath; path]

        let expected = [
            { SyncRule = Include; Path = subPath }, RuleReminder
            { SyncRule = NoRule; Path = path }, AddedToRepository
        ]
        result |> isAsExpected expected

module ``defineTrackedElements should`` =
    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``not remove elements that are not in scan result`` tracked =
        let result = defineTrackedElements tracked []
        test <@ Set result = Set tracked @>

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``add elements that are flag as added`` tracked element =
        let result = defineTrackedElements tracked [element, AddedToRepository]
        test <@ Set  result = Set (tracked@[element.Path]) @>

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``removed elements that are flag as removed`` tracked element =
        let result = defineTrackedElements (tracked@[element.Path]) [element, RemovedFromRepository]
        test <@ Set result = Set tracked @>
