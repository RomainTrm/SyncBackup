module SyncBackup.Tests.Infra.Content

open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Infra
open SyncBackup.Infra.Content

module Scan =
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

            Config.init testDirectoryPath {
                IsSourceRepository = true
                Aliases = []
                Rules = []
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

module ScanFile =
    let content = [
        File { Name = "file"; RelativePath = Alias "MyAlias\\file" }
        Directory { Name = "1. emptyDir"; RelativePath = Source "MySource\\1. emptyDir"; Content = [] }
        Directory { Name = "2. oneLevelDir"; RelativePath = Source "MySource\\2. oneLevelDir"; Content = [
            File { Name = "file1"; RelativePath = Source "MySource\\2. oneLevelDir\\file1" }
            File { Name = "file2"; RelativePath = Source "MySource\\2. oneLevelDir\\file2" }
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

    module ``writeFile should`` =
        let uniqueTestDirectory = "test-7c1e51c9-0eb0-4019-9268-faf20eddb0cb"

        [<Fact>]
        let ``save scan result`` () =
            let path = TestHelpers.testDirectoryPath uniqueTestDirectory
            TestHelpers.cleanupTests path
            TestHelpers.createDirectory [|uniqueTestDirectory|]
            TestHelpers.createDirectory [|uniqueTestDirectory; Dsl.ConfigDirectory|]

            let result = ScanFile.writeFile path content
            test <@ result = Ok () @>

            let fileContent = Dsl.getScanFileFilePath path |> System.IO.File.ReadAllLines
            let expected = [
                "norule (*file) \"MyAlias\\file\""
                "norule (directory) \"MySource\\1. emptyDir\""
                "norule (directory) \"MySource\\2. oneLevelDir\""
                "norule (file) \"MySource\\2. oneLevelDir\\file1\""
                "norule (file) \"MySource\\2. oneLevelDir\\file2\""
                "norule (*directory) \"MyAlias\\3. twoLevelsDir\""
                "norule (*directory) \"MyAlias\\3. twoLevelsDir\\subdir1\""
                "norule (*file) \"MyAlias\\3. twoLevelsDir\\subdir1\\file1\""
                "norule (*file) \"MyAlias\\3. twoLevelsDir\\subdir1\\file2\""
                "norule (*directory) \"MyAlias\\3. twoLevelsDir\\subdir2\""
                "norule (*file) \"MyAlias\\3. twoLevelsDir\\subdir2\\file\""
            ]

            test <@ (Set fileContent) |> Set.isSubset (Set expected)  @>

    module ``readFile should`` =
        [<Fact>]
        let ``should read file content`` () =
            let uniqueTestDirectory = "test-a864d347-0e6a-4e6c-aa70-877c9ce3adc6"
            let path = TestHelpers.testDirectoryPath uniqueTestDirectory
            TestHelpers.cleanupTests path
            TestHelpers.createDirectory [|uniqueTestDirectory|]
            TestHelpers.createDirectory [|uniqueTestDirectory; Dsl.ConfigDirectory|]

            let result = ScanFile.writeFile path content
            test <@ result = Ok () @>

            let result = ScanFile.readFile path ()
            let expected = [
                { SyncRule = NoRule; Path = Alias "MyAlias\\file" }
                { SyncRule = NoRule; Path = Source "MySource\\1. emptyDir" }
                { SyncRule = NoRule; Path = Source "MySource\\2. oneLevelDir" }
                { SyncRule = NoRule; Path = Source "MySource\\2. oneLevelDir\\file1" }
                { SyncRule = NoRule; Path = Source "MySource\\2. oneLevelDir\\file2" }
                { SyncRule = NoRule; Path = Alias "MyAlias\\3. twoLevelsDir" }
                { SyncRule = NoRule; Path = Alias "MyAlias\\3. twoLevelsDir\\subdir1" }
                { SyncRule = NoRule; Path = Alias "MyAlias\\3. twoLevelsDir\\subdir1\\file1" }
                { SyncRule = NoRule; Path = Alias "MyAlias\\3. twoLevelsDir\\subdir1\\file2" }
                { SyncRule = NoRule; Path = Alias "MyAlias\\3. twoLevelsDir\\subdir2" }
                { SyncRule = NoRule; Path = Alias "MyAlias\\3. twoLevelsDir\\subdir2\\file" }
            ]
            test <@ result = Ok expected @>

        [<Fact>]
        let ``should return error if invalid file content format`` () =
            let uniqueTestDirectory = "test-b694318b-35db-4f20-a42e-b2123d189773"
            let path = TestHelpers.testDirectoryPath uniqueTestDirectory
            TestHelpers.cleanupTests path
            TestHelpers.createDirectory [|uniqueTestDirectory|]
            TestHelpers.createDirectory [|uniqueTestDirectory; Dsl.ConfigDirectory|]

            let filePath = Dsl.getScanFileFilePath path
            System.IO.File.WriteAllText (filePath, "invalid")

            let result = ScanFile.readFile path ()
            test <@ result = Error "Invalid format" @>

module TrackFile =
    module ``save should`` =
        [<Fact>]
        let ``register tracked elements`` () =
            let uniqueTestDirectory = "test-7ac330ef-d9bb-41f7-bfdd-1bbec664c05a"
            let path = TestHelpers.testDirectoryPath uniqueTestDirectory
            TestHelpers.cleanupTests path
            TestHelpers.createDirectory [|uniqueTestDirectory|]
            TestHelpers.createDirectory [|uniqueTestDirectory; Dsl.ConfigDirectory|]

            let content = [Source "line1"; Alias "line2"; Alias "line3"]
            let result = TrackFile.save path content
            test <@ result = Ok () @>

            let fileContent = Dsl.getTrackFileFilePath path |> System.IO.File.ReadAllLines |> Seq.toList
            test <@ fileContent = ["line1"; "*line2"; "*line3"] @>
