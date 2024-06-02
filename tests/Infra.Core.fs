module SyncBackup.Tests.Infra.Core

open System
open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Infra.Core

let [<Literal>] private UniqueTestDirectory = "test-7a12f9f2-52db-4465-9cb8-0f37426dd13f"
let testDirectory = TestHelpers.testDirectoryPath UniqueTestDirectory

module ``checkPathExists should`` =
    [<Fact>]
    let ``return ok if exists`` () =
        let path = Environment.CurrentDirectory
        let result = checkPathExists path
        test <@ result = Ok () @>

    [<Fact>]
    let ``return error if doesn't exist`` () =
        let path = "wrong path"
        let result = checkPathExists path
        test <@ result = Error "The specified directory path doesn't exist" @>

module ``buildRelativePath should`` =
    open System.IO

    let setupContent () =
        if (not<<Directory.Exists) testDirectory
        then
            TestHelpers.createDirectory [|testDirectory|]
            TestHelpers.createDirectory [|testDirectory; "source"; "directory"|]
            TestHelpers.createFile [|testDirectory; "source"; "file"|]
            TestHelpers.createDirectory [|testDirectory; "alias"; "directory"|]
            TestHelpers.createFile [|testDirectory; "alias"; "file"|]

    [<Fact>]
    let ``return error if path doesn't exist`` () =
        let path = "wrong path"
        let result = buildRelativePath testDirectory [] path
        test <@ result = Error "The specified path doesn't exist" @>

    let ``return relative path - test cases`` () : obj[] list = [
        [| "directory"; { Value = "directory"; Type = Source; ContentType = ContentType.Directory } |]
        [| "file"; { Value = "file"; Type = Source; ContentType = ContentType.File } |]
        [| "alias\\directory"; { Value = "alias\\directory"; Type = Alias; ContentType = ContentType.Directory } |]
        [| "alias\\file"; { Value = "alias\\file"; Type = Alias; ContentType = ContentType.File } |]
    ]

    [<Theory; MemberData(nameof ``return relative path - test cases``)>]
    let ``return relative path`` (unverifiedPath: UnverifiedPath) (relativePath: RelativePath) =
        setupContent ()
        let alias: Alias = { Name = "alias"; Path = Path.Combine(testDirectory, "alias") }
        let repositoryRoot = Path.Combine(testDirectory, "source")
        let result = buildRelativePath repositoryRoot [alias] unverifiedPath
        test <@ result = Ok relativePath @>

    [<Fact>]
    let ``correct directory separator`` () =
        setupContent ()
        let alias: Alias = { Name = "alias"; Path = Path.Combine(testDirectory, "alias") }
        let repositoryRoot = Path.Combine(testDirectory, "source")
        let result = buildRelativePath repositoryRoot [alias] "alias/file"
        test <@ result = Ok { Value = "alias\\file"; Type = Alias; ContentType = ContentType.File } @>
