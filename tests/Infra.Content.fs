module SyncBackup.Tests.Infra.Content

open SyncBackup.Domain.Dsl
open Xunit
open Swensen.Unquote
open SyncBackup.Infra.Content

module ``scan should`` =
    let [<Literal>] private UniqueTestDirectory = "test-5f8aeeaa-0090-4687-a361-d6db230806f0"

    let private setupTestFolder () =
        let path = TestHelpers.testDirectoryPath UniqueTestDirectory
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
        setupTestFolder ()
        let result = scan (TestHelpers.testDirectoryPath UniqueTestDirectory) []
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
