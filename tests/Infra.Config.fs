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
