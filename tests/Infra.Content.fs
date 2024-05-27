﻿module SyncBackup.Tests.Infra.Content

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
            let expected : RelativePath list = [
                { Type = Source; Value = "file"; ContentType = File }
                { Type = Source; Value = "1. emptyDir"; ContentType = Directory }
                { Type = Source; Value = "2. oneLevelDir"; ContentType = Directory }
                { Type = Source; Value = "2. oneLevelDir\\file1"; ContentType = File }
                { Type = Source; Value = "2. oneLevelDir\\file2"; ContentType = File }
                { Type = Source; Value = "3. twoLevelsDir"; ContentType = Directory }
                { Type = Source; Value = "3. twoLevelsDir\\subdir1"; ContentType = Directory }
                { Type = Source; Value = "3. twoLevelsDir\\subdir1\\file1"; ContentType = File }
                { Type = Source; Value = "3. twoLevelsDir\\subdir1\\file2"; ContentType = File }
                { Type = Source; Value = "3. twoLevelsDir\\subdir2"; ContentType = Directory }
                { Type = Source; Value = "3. twoLevelsDir\\subdir2\\file"; ContentType = File }
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

            let expected : RelativePath list = [
                { Type = Alias; Value = "MyAlias\\file"; ContentType = File }
                { Type = Alias; Value = "MyAlias\\1. emptyDir"; ContentType = Directory }
                { Type = Alias; Value = "MyAlias\\2. oneLevelDir"; ContentType = Directory }
                { Type = Alias; Value = "MyAlias\\2. oneLevelDir\\file1"; ContentType = File }
                { Type = Alias; Value = "MyAlias\\2. oneLevelDir\\file2"; ContentType = File }
                { Type = Alias; Value = "MyAlias\\3. twoLevelsDir"; ContentType = Directory }
                { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1"; ContentType = Directory }
                { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1\\file1"; ContentType = File }
                { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1\\file2"; ContentType = File }
                { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir2"; ContentType = Directory }
                { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir2\\file"; ContentType = File }
            ]
            test <@ result = expected @>

module ScanFile =
    let content = [
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\file"; ContentType = File } }, RemovedFromRepository
        { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\1. emptyDir"; ContentType = Directory } }, AddedToRepository
        { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\2. oneLevelDir"; ContentType = Directory } }, RuleReminder
        { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\2. oneLevelDir\\file1"; ContentType = File } }, AddedToRepository
        { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\2. oneLevelDir\\file2"; ContentType = File } }, AddedToRepository
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir"; ContentType = Directory } }, AddedToRepository
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1"; ContentType = Directory } }, AddedToRepository
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1\\file1"; ContentType = File } }, AddedToRepository
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1\\file2"; ContentType = File } }, AddedToRepository
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir2"; ContentType = Directory } }, AddedToRepository
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir2\\file"; ContentType = File } }, AddedToRepository
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
                "norule (removed) file::\"*MyAlias\\file\""
                "norule (added) dir::\"MySource\\1. emptyDir\""
                "# norule (nochange) dir::\"MySource\\2. oneLevelDir\""
                "norule (added) file::\"MySource\\2. oneLevelDir\\file1\""
                "norule (added) file::\"MySource\\2. oneLevelDir\\file2\""
                "norule (added) dir::\"*MyAlias\\3. twoLevelsDir\""
                "norule (added) dir::\"*MyAlias\\3. twoLevelsDir\\subdir1\""
                "norule (added) file::\"*MyAlias\\3. twoLevelsDir\\subdir1\\file1\""
                "norule (added) file::\"*MyAlias\\3. twoLevelsDir\\subdir1\\file2\""
                "norule (added) dir::\"*MyAlias\\3. twoLevelsDir\\subdir2\""
                "norule (added) file::\"*MyAlias\\3. twoLevelsDir\\subdir2\\file\""
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

            let result = ScanFile.readFile path
            let expected = [
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\file"; ContentType = File } }, RemovedFromRepository
                { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\1. emptyDir"; ContentType = Directory } }, AddedToRepository
                { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\2. oneLevelDir\\file1"; ContentType = File } }, AddedToRepository
                { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\2. oneLevelDir\\file2"; ContentType = File } }, AddedToRepository
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir"; ContentType = Directory } }, AddedToRepository
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1"; ContentType = Directory } }, AddedToRepository
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1\\file1"; ContentType = File } }, AddedToRepository
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1\\file2"; ContentType = File } }, AddedToRepository
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir2"; ContentType = Directory } }, AddedToRepository
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir2\\file"; ContentType = File } }, AddedToRepository
            ]
            test <@ result = Ok expected @>

        [<Theory>]
        [<InlineData("invalid")>]
        [<InlineData("norule")>]
        let ``should return error if invalid file content format`` (line: string) =
            let uniqueTestDirectory = "test-b694318b-35db-4f20-a42e-b2123d189773"
            let path = TestHelpers.testDirectoryPath uniqueTestDirectory
            TestHelpers.cleanupTests path
            TestHelpers.createDirectory [|uniqueTestDirectory|]
            TestHelpers.createDirectory [|uniqueTestDirectory; Dsl.ConfigDirectory|]

            let filePath = Dsl.getScanFileFilePath path
            System.IO.File.WriteAllText (filePath, line)

            let result = ScanFile.readFile path
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

            let content = [
                { Type = Source; Value = "line1"; ContentType = File }
                { Type = Alias; Value = "line2"; ContentType = File }
                { Type = Alias; Value = "line3"; ContentType = Directory }
            ]
            let result = TrackFile.save path content
            test <@ result = Ok () @>

            let fileContent = Dsl.getTrackFileFilePath path |> System.IO.File.ReadAllLines |> Seq.toList
            test <@ fileContent = [
                "file::\"line1\""
                "file::\"*line2\""
                "dir::\"*line3\""
            ] @>

    module ``read should`` =
        [<Fact>]
        let ``return file content`` () =
            let uniqueTestDirectory = "test-86cc56ee-cbb5-4aab-8c0d-f61f54744cb3"
            let path = TestHelpers.testDirectoryPath uniqueTestDirectory
            TestHelpers.cleanupTests path
            TestHelpers.createDirectory [|uniqueTestDirectory|]
            TestHelpers.createDirectory [|uniqueTestDirectory; Dsl.ConfigDirectory|]

            let content = [
                { Type = Source; Value = "line1"; ContentType = File }
                { Type = Alias; Value = "line2"; ContentType = File }
                { Type = Alias; Value = "line3"; ContentType = Directory }
            ]
            let result = TrackFile.save path content
            test <@ result = Ok () @>

            let result = TrackFile.load path
            test <@ result = Ok content @>

        [<Fact>]
        let ``return empty when file not created`` () =
            let uniqueTestDirectory = "test-25903348-b5ec-4a26-a257-dbc2cb7dfcf2"
            let path = TestHelpers.testDirectoryPath uniqueTestDirectory
            TestHelpers.cleanupTests path
            TestHelpers.createDirectory [|uniqueTestDirectory|]
            TestHelpers.createDirectory [|uniqueTestDirectory; Dsl.ConfigDirectory|]

            let result = TrackFile.load path
            test <@ result = Ok [] @>
