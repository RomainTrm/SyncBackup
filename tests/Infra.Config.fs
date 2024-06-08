module SyncBackup.Tests.Infra.Config

open System
open System.IO
open Xunit
open FsCheck.Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Infra.Dsl
open SyncBackup.Infra.Config

let [<Literal>] private UniqueTestDirectory = "test-a7922ff5-86af-466b-8adb-d11257e9f0ee"
let cleanupTests = TestHelpers.cleanupTests UniqueTestDirectory // Run once before tests execution

let generateRepositoryPath () = Path.Combine(TestHelpers.currentDirectory, UniqueTestDirectory, Guid.NewGuid().ToString())
let configDirectoryPath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory)
let configFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, ConfigFile)
let readConfigFile = configFilePath >> File.ReadAllLines

module ``init should`` =
    let defaultConfig : RepositoryConfig = {
        Version = RepositoryConfigVersion
        Type = RepositoryType.Source
        Aliases = []
        Rules = []
    }

    [<Property(MaxTest = 10)>]
    let ``create config file`` repositoryType =
        let repositoryPath = generateRepositoryPath ()
        let expectedConfig = { defaultConfig with Type = repositoryType }
        let result = init repositoryPath expectedConfig
        test <@ result = Ok () @>
        test <@ load repositoryPath = Ok expectedConfig @>

    [<Fact>]
    let ``create config file when directory exists`` () =
        let repositoryPath = generateRepositoryPath ()
        configDirectoryPath repositoryPath |> Directory.CreateDirectory |> ignore
        let result = init repositoryPath defaultConfig
        test <@ result = Ok () @>
        test <@ load repositoryPath = Ok defaultConfig @>

    [<Fact>]
    let ``return error if config file already exists`` () =
        let repositoryPath = generateRepositoryPath ()
        let _ = init repositoryPath defaultConfig
        let result = init repositoryPath defaultConfig
        test <@ result = Error "A repository is already initialized here" @>

module ``load should`` =
    [<Fact>]
    let ``return error if config file doesn't exist`` () =
        let path = "wrong path"
        let result = load path
        test <@ result = Error "No repository in the current directory" @>

    [<Property(MaxTest = 10)>]
    let ``return config file`` configFile =
        let repositoryPath = generateRepositoryPath ()
        let _ = init repositoryPath configFile
        let result = load repositoryPath
        test <@ result = Ok configFile @>

module ``update should`` =
    [<Property(MaxTest = 10)>]
    let ``return error if config file doesn't exist`` configFile =
        let path = "wrong path"
        let result = update path configFile
        test <@ result = Error "No repository in the current directory" @>

    [<Property(MaxTest = 10)>]
    let ``persist new value`` oldConfigFile newConfigFile =
        let repositoryPath = generateRepositoryPath ()
        let _ = init repositoryPath oldConfigFile
        let result = update repositoryPath newConfigFile
        test <@ result = Ok () @>
        test <@ load repositoryPath = Ok newConfigFile @>

module RuleEditionFile =
    let content = [
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\file"; ContentType = File } }
        { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\1. emptyDir"; ContentType = Directory } }
        { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\2. oneLevelDir"; ContentType = Directory } }
        { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\2. oneLevelDir\\file1"; ContentType = File } }
        { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\2. oneLevelDir\\file2"; ContentType = File } }
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir"; ContentType = Directory } }
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1"; ContentType = Directory } }
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1\\file1"; ContentType = File } }
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1\\file2"; ContentType = File } }
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir2"; ContentType = Directory } }
        { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir2\\file"; ContentType = File } }
    ]

    module ``saveFile should`` =
        [<Fact>]
        let ``save scan result`` () =
            let uniqueTestDirectory = "test-1e004371-b86a-4032-8ed5-c94c53e1ef52"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let result = RuleEditionFile.saveFile path RepositoryType.Source content
            test <@ result = Ok () @>

            let fileContent = getRulesEditionFilePath path |> System.IO.File.ReadAllLines
            let expected = [
                "norule file::\"*MyAlias\\file\""
                "norule dir::\"MySource\\1. emptyDir\""
                "norule dir::\"MySource\\2. oneLevelDir\""
                "norule file::\"MySource\\2. oneLevelDir\\file1\""
                "norule file::\"MySource\\2. oneLevelDir\\file2\""
                "norule dir::\"*MyAlias\\3. twoLevelsDir\""
                "norule dir::\"*MyAlias\\3. twoLevelsDir\\subdir1\""
                "norule file::\"*MyAlias\\3. twoLevelsDir\\subdir1\\file1\""
                "norule file::\"*MyAlias\\3. twoLevelsDir\\subdir1\\file2\""
                "norule dir::\"*MyAlias\\3. twoLevelsDir\\subdir2\""
                "norule file::\"*MyAlias\\3. twoLevelsDir\\subdir2\\file\""
            ]

            test <@ (Set fileContent) |> Set.isSubset (Set expected)  @>

        [<Fact>]
        let ``empty file result should display "empty repository"`` () =
            let uniqueTestDirectory = "test-ca3d8846-3d8f-41a2-b581-ef30bb58175d"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let result = RuleEditionFile.saveFile path RepositoryType.Source []
            test <@ result = Ok () @>

            let fileContent = getRulesEditionFilePath path |> System.IO.File.ReadAllLines
            test <@ fileContent |> Seq.contains "# Your repository is empty, no rule to edit."  @>

    module ``readFile should`` =
        [<Fact>]
        let ``should read file content`` () =
            let uniqueTestDirectory = "test-618fa778-92d4-470b-bbcb-6c5507f959a8"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let result = RuleEditionFile.saveFile path RepositoryType.Source content
            test <@ result = Ok () @>

            let result = RuleEditionFile.loadFile path
            let expected = [
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\file"; ContentType = File } }
                { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\1. emptyDir"; ContentType = Directory } }
                { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\2. oneLevelDir"; ContentType = Directory } }
                { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\2. oneLevelDir\\file1"; ContentType = File } }
                { SyncRule = NoRule; Path = { Type = Source; Value = "MySource\\2. oneLevelDir\\file2"; ContentType = File } }
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir"; ContentType = Directory } }
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1"; ContentType = Directory } }
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1\\file1"; ContentType = File } }
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir1\\file2"; ContentType = File } }
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir2"; ContentType = Directory } }
                { SyncRule = NoRule; Path = { Type = Alias; Value = "MyAlias\\3. twoLevelsDir\\subdir2\\file"; ContentType = File } }
            ]
            test <@ result = Ok expected @>

        [<Theory>]
        [<InlineData("invalid")>]
        [<InlineData("norule")>]
        let ``should return error if invalid file content format`` (line: string) =
            let uniqueTestDirectory = "test-6e3b3cd3-ca5e-4fe4-a64a-ac0e1a607204"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let filePath = getRulesEditionFilePath path
            System.IO.File.WriteAllText (filePath, line)

            let result = RuleEditionFile.loadFile path
            test <@ result = Error "Invalid format" @>
