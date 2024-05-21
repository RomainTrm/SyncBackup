module SyncBackup.Tests.Infra.Config

open System
open System.IO
open FsCheck.Xunit
open SyncBackup.Domain.Dsl
open Xunit
open Swensen.Unquote
open SyncBackup.Infra.Dsl
open SyncBackup.Infra.Config

let repositoryPath = Environment.CurrentDirectory
let configDirectoryPath = Path.Combine(repositoryPath, ConfigDirectory)
let configFilePath = Path.Combine(repositoryPath, ConfigDirectory, ConfigFile)

let deleteRepository () =
    if Directory.Exists configDirectoryPath
    then Directory.Delete(configDirectoryPath, true)

let readConfigFile () = File.ReadAllLines configFilePath

module ``init should`` =
    let defaultConfig : RepositoryConfig = {
        IsSourceRepository = true
        Aliases = []
    }

    [<Theory>]
    [<InlineData(true)>]
    [<InlineData(false)>]
    let ``create config file`` isSourceRepository =
        deleteRepository ()
        let expectedConfig = { defaultConfig with IsSourceRepository = isSourceRepository }
        let result = init repositoryPath expectedConfig
        test <@ result = Ok () @>
        test <@ load repositoryPath = Ok expectedConfig @>

    [<Fact>]
    let ``create config file when directory exists`` () =
        deleteRepository ()
        Directory.CreateDirectory configDirectoryPath |> ignore
        let result = init repositoryPath defaultConfig
        test <@ result = Ok () @>
        test <@ load repositoryPath = Ok defaultConfig @>

    [<Fact>]
    let ``return error if config file already exists`` () =
        deleteRepository ()

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
        deleteRepository ()

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
        deleteRepository ()
        let _ = init repositoryPath oldConfigFile
        let result = update repositoryPath newConfigFile
        test <@ result = Ok () @>
        test <@ load repositoryPath = Ok newConfigFile @>

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
