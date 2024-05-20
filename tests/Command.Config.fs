module SyncBackup.Tests.Command.Config

open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Commands.Config

let defaultInfra : Infra = {
    InitConfig = fun _ _ -> failwith "not implemented"
    LoadConfig = fun _ -> failwith "not implemented"
    UpdateConfig = fun _ _ -> failwith "not implemented"
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
        let expectedConfig: RepositoryConfig = {
            IsMainRepository = true
            Aliases = []
        }
        test <@ calls |> Seq.toList = [ path, expectedConfig ] @>

module Aliases =
    let path = "some path"

    let defaultConfig : RepositoryConfig = {
        IsMainRepository = true
        Aliases = []
    }

    module ``add should`` =
        [<Fact>]
        let ``add alias to configuration`` () =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    LoadConfig = fun repoPath ->
                        test <@ repoPath = path @>
                        Ok { defaultConfig with Aliases = [] }
                    UpdateConfig = fun repoPath repoConfig ->
                        calls.Add (repoPath, repoConfig)
                        Ok ()
            }

            let alias = { Name = "alias name"; Path = "directory path" }
            let result = Alias.add infra path alias

            test <@ result = Ok () @>
            test <@ calls |> Seq.toList = [ path, { defaultConfig with Aliases = [ alias ] } ] @>

        [<Fact>]
        let ``add alias to configuration with existing aliases`` () =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    LoadConfig = fun repoPath ->
                        test <@ repoPath = path @>
                        Ok {
                            defaultConfig with
                                Aliases = [
                                    { Name = "alias 1"; Path = "path 1" }
                                    { Name = "alias 2"; Path = "path 2" }
                                ]
                            }
                    UpdateConfig = fun repoPath repoConfig ->
                        calls.Add (repoPath, repoConfig)
                        Ok ()
            }

            let alias = { Name = "alias 3"; Path = "path 3" }
            let result = Alias.add infra path alias

            test <@ result = Ok () @>
            let expectedConfig = {
                defaultConfig with
                    Aliases = [
                        { Name = "alias 1"; Path = "path 1" }
                        { Name = "alias 2"; Path = "path 2" }
                        alias
                    ]
            }
            test <@ calls |> Seq.toList = [ path, expectedConfig ] @>

        [<Fact>]
        let ``do nothing if alias already exists`` () =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    LoadConfig = fun repoPath ->
                        test <@ repoPath = path @>
                        Ok {
                            defaultConfig with
                                Aliases = [
                                    { Name = "alias 1"; Path = "path 1" }
                                    { Name = "alias 2"; Path = "path 2" }
                                ]
                            }
                    UpdateConfig = fun repoPath repoConfig ->
                        calls.Add (repoPath, repoConfig)
                        Ok ()
            }

            let alias = { Name = "alias 1"; Path = "path 1" }
            let result = Alias.add infra path alias

            test <@ result = Ok () @>
            test <@ calls |> Seq.isEmpty @>

        // TODO : missing config
        // TODO : duplicated alias name -> return error
