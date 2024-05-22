module SyncBackup.Tests.Infra.Content

open SyncBackup.Domain.Dsl
open Xunit
open Swensen.Unquote
open SyncBackup.Infra.Content

module ``scan should`` =
    let private setupTestFolder uniqueTestDirectory =
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
        setupTestFolder uniqueTestDirectory
        let result = scan (TestHelpers.testDirectoryPath uniqueTestDirectory) []
        let expected : Content list = [
            File { Name = "file"; RelativePath = "file" }
            Directory { Name = "1. emptyDir"; RelativePath = "1. emptyDir"; Content = [] }
            Directory { Name = "2. oneLevelDir"; RelativePath = "2. oneLevelDir"; Content = [
                File { Name = "file1"; RelativePath = "2. oneLevelDir\\file1" }
                File { Name = "file2"; RelativePath = "2. oneLevelDir\\file2" }
            ] }
            Directory { Name = "3. twoLevelsDir"; RelativePath = "3. twoLevelsDir"; Content = [
                Directory { Name = "subdir1"; RelativePath = "3. twoLevelsDir\\subdir1"; Content = [
                    File { Name = "file1"; RelativePath = "3. twoLevelsDir\\subdir1\\file1"; }
                    File { Name = "file2"; RelativePath = "3. twoLevelsDir\\subdir1\\file2"; }
                ] }
                Directory { Name = "subdir2"; RelativePath = "3. twoLevelsDir\\subdir2"; Content = [
                    File { Name = "file"; RelativePath = "3. twoLevelsDir\\subdir2\\file"; }
                ] }
            ] }
        ]
        test <@ result = expected @>

    [<Fact>]
    let ``ignore config directory`` () =
        let uniqueTestDirectory = "test-e32b76b0-c553-4365-8195-2b91aeb29fd3"
        TestHelpers.cleanupTests uniqueTestDirectory
        let testDirectoryPath = TestHelpers.testDirectoryPath uniqueTestDirectory

        SyncBackup.Infra.Config.init testDirectoryPath {
            IsSourceRepository = true
            Aliases = []
        } |> ignore<Result<unit, string>>

        let result = scan testDirectoryPath []
        test <@ result = [] @>

