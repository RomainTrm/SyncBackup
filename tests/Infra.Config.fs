module SyncBackup.Tests.Infra.Config

open System
open System.IO
open SyncBackup.Domain.Dsl
open Xunit
open Swensen.Unquote
open SyncBackup.Infra.Dsl
open SyncBackup.Infra.Config

module ``init should`` =
    let defaultConfig : RepositoryConfig = {
        IsMainRepository = true
        Aliases = []
    }

    let repositoryPath = Environment.CurrentDirectory
    let directoryPath = Path.Combine(repositoryPath, ConfigDirectory)
    let configFilePath = Path.Combine(repositoryPath, ConfigDirectory, ConfigFile)

    let deleteRepository () =
        if Directory.Exists directoryPath
        then Directory.Delete(directoryPath, true)

    let readConfigFile () = File.ReadAllLines configFilePath

    [<Theory>]
    [<InlineData(true, "true")>]
    [<InlineData(false, "false")>]
    let ``create config file`` isMainRepository isMainRepositorySerialized =
        deleteRepository ()
        let result = init repositoryPath { defaultConfig with IsMainRepository = isMainRepository }
        test <@ result = Ok () @>

        let expectedFileContent = [|
            "[main]"
            $"\tisMainRepository = {isMainRepositorySerialized}"
        |]
        test <@ readConfigFile () = expectedFileContent @>

    [<Fact>]
    let ``create config file when directory exists`` () =
        deleteRepository ()
        Directory.CreateDirectory directoryPath |> ignore
        let result = init repositoryPath defaultConfig
        test <@ result = Ok () @>

        let expectedFileContent = [|
            "[main]"
            "\tisMainRepository = true"
        |]
        test <@ readConfigFile () = expectedFileContent @>

    [<Fact>]
    let ``return error if config file already exists`` () =
        deleteRepository ()

        let _ = init repositoryPath defaultConfig
        let result = init repositoryPath defaultConfig
        test <@ result = Error "A repository is already initialized here" @>
