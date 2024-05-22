module SyncBackup.Tests.Infra.Content

open SyncBackup.Domain.Dsl
open Xunit
open Swensen.Unquote
open SyncBackup.Infra.Content

module ``scan should`` =
    let private setupDirectoryContent uniqueTestDirectory =
        let path = TestHelpers.testDirectoryPath uniqueTestDirectory
        TestHelpers.cleanupTests path
        TestHelpers.createDirectory [|path|]
        TestHelpers.createFile [|path; "file"|]
        TestHelpers.createDirectory [|path; "1. emptyDir"|]
        TestHelpers.createDirectory [|path; "2. oneLevelDir"|]
        TestHelpers.createFile [|path; "2. oneLevelDir"; "file1"|]
        TestHelpers.createFile [|path; "2. oneLevelDir"; "file2"|]
        TestHelpers.createDirectory [|path; "3. twoLevelsDir"|]
        TestHelpers.createDirectory [|path; "3. twoLevelsDir"; "subdir1"|]
        TestHelpers.createFile [|path; "3. twoLevelsDir"; "subdir1"; "file1"|]
        TestHelpers.createFile [|path; "3. twoLevelsDir"; "subdir1"; "file2"|]
        TestHelpers.createDirectory [|path; "3. twoLevelsDir"; "subdir2"|]
        TestHelpers.createFile [|path; "3. twoLevelsDir"; "subdir2"; "file"|]

    [<Fact>]
    let ``return current directory content`` () =
        let uniqueTestDirectory = "test-5f8aeeaa-0090-4687-a361-d6db230806f0"
        setupDirectoryContent uniqueTestDirectory
        let result = Scan.run (TestHelpers.testDirectoryPath uniqueTestDirectory) []
        let expected : Content list = [
            File { Name = "file"; RelativePath = Source "file" }
            Directory { Name = "1. emptyDir"; RelativePath = Source "1. emptyDir"; Content = [] }
            Directory { Name = "2. oneLevelDir"; RelativePath = Source "2. oneLevelDir"; Content = [
                File { Name = "file1"; RelativePath = Source "2. oneLevelDir\\file1" }
                File { Name = "file2"; RelativePath = Source "2. oneLevelDir\\file2" }
            ] }
            Directory { Name = "3. twoLevelsDir"; RelativePath = Source "3. twoLevelsDir"; Content = [
                Directory { Name = "subdir1"; RelativePath = Source "3. twoLevelsDir\\subdir1"; Content = [
                    File { Name = "file1"; RelativePath = Source "3. twoLevelsDir\\subdir1\\file1"; }
                    File { Name = "file2"; RelativePath = Source "3. twoLevelsDir\\subdir1\\file2"; }
                ] }
                Directory { Name = "subdir2"; RelativePath = Source "3. twoLevelsDir\\subdir2"; Content = [
                    File { Name = "file"; RelativePath = Source "3. twoLevelsDir\\subdir2\\file"; }
                ] }
            ] }
        ]
        test <@ result = expected @>

    [<Fact>]
    let ``ignore config directory`` () =
        let uniqueTestDirectory = "test-e32b76b0-c553-4365-8195-2b91aeb29fd3"
        TestHelpers.cleanupTests uniqueTestDirectory
        let testDirectoryPath = TestHelpers.testDirectoryPath uniqueTestDirectory
        TestHelpers.createDirectory [|testDirectoryPath|]

        SyncBackup.Infra.Config.init testDirectoryPath {
            IsSourceRepository = true
            Aliases = []
        } |> ignore<Result<unit, string>>

        let result = Scan.run testDirectoryPath []
        test <@ result = [] @>

    [<Fact>]
    let ``return aliases content`` () =
        let uniqueTestDirectory = "test-c6b56818-5d52-43d0-94af-791b0f26a8b5"
        let uniqueTestAliasDirectory = "test-112d8bfa-0c5a-4239-8656-558d49ac6390"
        TestHelpers.cleanupTests uniqueTestDirectory
        TestHelpers.createDirectory [|uniqueTestDirectory|]
        setupDirectoryContent uniqueTestAliasDirectory

        let result = Scan.run (TestHelpers.testDirectoryPath uniqueTestDirectory) [
            { Name = "MyAlias"; Path = TestHelpers.testDirectoryPath uniqueTestAliasDirectory }
        ]

        let expected : Content list = [
            File { Name = "file"; RelativePath = Alias "MyAlias\\file" }
            Directory { Name = "1. emptyDir"; RelativePath = Alias "MyAlias\\1. emptyDir"; Content = [] }
            Directory { Name = "2. oneLevelDir"; RelativePath = Alias "MyAlias\\2. oneLevelDir"; Content = [
                File { Name = "file1"; RelativePath = Alias "MyAlias\\2. oneLevelDir\\file1" }
                File { Name = "file2"; RelativePath = Alias "MyAlias\\2. oneLevelDir\\file2" }
            ] }
            Directory { Name = "3. twoLevelsDir"; RelativePath = Alias "MyAlias\\3. twoLevelsDir"; Content = [
                Directory { Name = "subdir1"; RelativePath = Alias "MyAlias\\3. twoLevelsDir\\subdir1"; Content = [
                    File { Name = "file1"; RelativePath = Alias "MyAlias\\3. twoLevelsDir\\subdir1\\file1"; }
                    File { Name = "file2"; RelativePath = Alias "MyAlias\\3. twoLevelsDir\\subdir1\\file2"; }
                ] }
                Directory { Name = "subdir2"; RelativePath = Alias "MyAlias\\3. twoLevelsDir\\subdir2"; Content = [
                    File { Name = "file"; RelativePath = Alias "MyAlias\\3. twoLevelsDir\\subdir2\\file"; }
                ] }
            ] }
        ]
        test <@ result = expected @>
