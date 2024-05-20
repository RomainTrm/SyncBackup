module SyncBackup.Tests.Command.Config

open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Commands.Config

let defaultInfra : Infra = {
    InitConfig = fun _ -> failwith "not implemented"
}

module ``Init should`` =

    [<Fact>]
    let ``ìnit repository for specified path`` () =
        let calls = System.Collections.Generic.List<_> ()
        let infra = {
            defaultInfra with
                InitConfig = fun repoPath repoConfig ->
                    calls.Add (repoPath, repoConfig)
                    Ok ()
        }

        let path = "some path"
        let result = Init.run infra path

        test <@ result = Ok () @>
        let expectedConfig: RepositoryConfig = { IsMainRepository = true }
        test <@ calls |> Seq.toList = [ path, expectedConfig ] @>
