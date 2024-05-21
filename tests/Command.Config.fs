module SyncBackup.Tests.Command.Config

open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Commands.Config

let defaultInfra : Infra = {
    InitConfig = fun _ -> failwith "not implemented"
    LoadConfig = fun _ -> failwith "not implemented"
    CheckPathExists = fun _ -> failwith "not implemented"
    UpdateConfig = fun _ -> failwith "not implemented"
}

module ``Init should`` =

    [<Fact>]
    let ``ìnit repository`` () =
        let calls = System.Collections.Generic.List<_> ()
        let infra = {
            defaultInfra with
                InitConfig = calls.Add >> Ok
        }

        let result = Init.run infra

        test <@ result = Ok () @>
        let expectedConfig: RepositoryConfig = {
            IsSourceRepository = true
            Aliases = []
        }
        test <@ calls |> Seq.toList = [ expectedConfig ] @>

module Aliases =
    let path = "some path"

    let defaultConfig : RepositoryConfig = {
        IsSourceRepository = true
        Aliases = []
    }

    module ``add should`` =
        [<Fact>]
        let ``add alias to configuration`` () =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    CheckPathExists = fun _ -> Ok ()
                    LoadConfig = fun () ->
                        Ok { defaultConfig with Aliases = [] }
                    UpdateConfig = calls.Add >> Ok
            }

            let alias = { Name = "alias name"; Path = "directory path" }
            let result = Alias.add infra alias

            test <@ result = Ok () @>
            test <@ calls |> Seq.toList = [ { defaultConfig with Aliases = [ alias ] } ] @>

        [<Fact>]
        let ``add alias to configuration with existing aliases`` () =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    CheckPathExists = fun _ -> Ok ()
                    LoadConfig = fun () ->
                        Ok {
                            defaultConfig with
                                Aliases = [
                                    { Name = "alias 1"; Path = "path 1" }
                                    { Name = "alias 2"; Path = "path 2" }
                                ]
                            }
                    UpdateConfig = calls.Add >> Ok
            }

            let alias = { Name = "alias 3"; Path = "path 3" }
            let result = Alias.add infra alias

            test <@ result = Ok () @>
            let expectedConfig = {
                defaultConfig with
                    Aliases = [
                        { Name = "alias 1"; Path = "path 1" }
                        { Name = "alias 2"; Path = "path 2" }
                        alias
                    ]
            }
            test <@ calls |> Seq.toList = [ expectedConfig ] @>

        [<Fact>]
        let ``do nothing if alias already exists`` () =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    CheckPathExists = fun _ -> Ok ()
                    LoadConfig = fun () ->
                        Ok {
                            defaultConfig with
                                Aliases = [
                                    { Name = "alias 1"; Path = "path 1" }
                                    { Name = "alias 2"; Path = "path 2" }
                                ]
                            }
                    UpdateConfig = calls.Add >> Ok
            }

            let alias = { Name = "alias 1"; Path = "path 1" }
            let result = Alias.add infra alias

            test <@ result = Ok () @>
            test <@ calls |> Seq.isEmpty @>

        [<Fact>]
        let ``return error if alias name exists`` () =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    CheckPathExists = fun _ -> Ok ()
                    LoadConfig = fun () ->
                        Ok {
                            defaultConfig with
                                Aliases = [
                                    { Name = "alias 1"; Path = "path 1" }
                                    { Name = "alias 2"; Path = "path 2" }
                                ]
                            }
                    UpdateConfig = calls.Add >> Ok
            }

            let alias = { Name = "alias 1"; Path = "path 3" }
            let result = Alias.add infra alias

            test <@ result = Error """The alias "alias 1" already exists for another directory.""" @>
            test <@ calls |> Seq.isEmpty @>

        [<Fact>]
        let ``return error if alias path doesn't exist`` () =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    CheckPathExists = function
                        | "path" -> Error "error message"
                        | _ -> failwith "not implemented"
                    UpdateConfig = calls.Add >> Ok
            }

            let alias = { Name = "alias 1"; Path = "path" }
            let result = Alias.add infra alias

            test <@ result = Error "error message" @>
            test <@ calls |> Seq.isEmpty @>

        [<Theory>]
        [<InlineData('\\')>]
        [<InlineData('/')>]
        [<InlineData(':')>]
        [<InlineData('*')>]
        [<InlineData('?')>]
        [<InlineData('"')>]
        [<InlineData('<')>]
        [<InlineData('>')>]
        [<InlineData('|')>]
        let ``return error if alias name contains invalid chars`` (invalidChar: char) =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    UpdateConfig = calls.Add >> Ok
            }

            let alias = { Name = $"alias{invalidChar}1"; Path = "path" }
            let result = Alias.add infra alias

            test <@ result = Error "Alias name contains forbidden characters (\\/:*?\"<>|)" @>
            test <@ calls |> Seq.isEmpty @>

        [<Fact>]
        let ``return error if in a backup repository`` () =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    CheckPathExists = fun _ -> Ok ()
                    LoadConfig = fun () ->
                        Ok { defaultConfig with IsSourceRepository = false }
                    UpdateConfig = calls.Add >> Ok
            }

            let alias = { Name = "alias 1"; Path = "path" }
            let result = Alias.add infra alias

            test <@ result = Error "Aliases are only supported by source repositories" @>
            test <@ calls |> Seq.isEmpty @>
