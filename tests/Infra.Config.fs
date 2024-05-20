module SyncBackup.Tests.Infra.Config

open System
open System.IO
open SyncBackup.Domain.Dsl
open Xunit
open Swensen.Unquote
open SyncBackup.Infra.Dsl
open SyncBackup.Infra.Config

module ``init should`` =
    let config : RepositoryConfig = {
        IsMainRepository = true
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
        let result = init repositoryPath { IsMainRepository = isMainRepository }
        test <@ result = Ok () @>

        let expectedFileContent = [|
            "[Main]"
            $"IsMainRepository = {isMainRepositorySerialized}"
        |]
        test <@ readConfigFile () = expectedFileContent @>

    [<Fact>]
    let ``create config file when directory exists`` () =
        deleteRepository ()
        Directory.CreateDirectory directoryPath |> ignore
        let result = init repositoryPath config
        test <@ result = Ok () @>

        let expectedFileContent = [|
            "[Main]"
            "IsMainRepository = true"
        |]
        test <@ readConfigFile () = expectedFileContent @>

    [<Fact>]
    let ``return error if config file already exists`` () =
        deleteRepository ()

        let _ = init repositoryPath config
        let result = init repositoryPath config
        test <@ result = Error "A repository is already initialized here" @>
