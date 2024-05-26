module SyncBackup.Tests.Commands.Config

open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Commands.Config

let defaultInfra : Infra = {
    InitConfig = fun _ -> failwith "not implemented"
    LoadConfig = fun _ -> failwith "not implemented"
    CheckPathExists = fun _ -> failwith "not implemented"
    UpdateConfig = fun _ -> failwith "not implemented"
    SolveRuleConflict = fun _ _ -> failwith "not implemented"
}

let defaultConfig : RepositoryConfig = {
    IsSourceRepository = true
    Aliases = []
    Rules = []
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
            Rules = []
        }
        test <@ calls |> Seq.toList = [ expectedConfig ] @>

module Aliases =
    let path = "some path"

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

module Rules =
    module ``Add should`` =
        [<Fact>]
        let ``add rule to config`` () =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    LoadConfig = fun () ->
                        Ok { defaultConfig with Rules = [] }
                    UpdateConfig = calls.Add >> Ok
            }

            let rule = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
            let result = Rules.add infra rule

            test <@ result = Ok () @>
            test <@ calls |> Seq.toList = [ { defaultConfig with Rules = [ rule ] } ] @>

        [<Fact>]
        let ``add rule to config with existing rules`` () =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    LoadConfig = fun () ->
                        Ok {
                            defaultConfig with
                                Rules = [
                                    { Path = { Type = Source; Value = "path1"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
                                    { Path = { Type = Source; Value = "path2"; ContentType = Directory }; SyncRule = SyncRules.Include }
                                ]
                        }
                    UpdateConfig = calls.Add >> Ok
            }

            let rule = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
            let result = Rules.add infra rule

            test <@ result = Ok () @>
            test <@ calls |> Seq.toList = [ {
                defaultConfig with
                    Rules = [
                        { Path = { Type = Source; Value = "path1"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
                        { Path = { Type = Source; Value = "path2"; ContentType = Directory }; SyncRule = SyncRules.Include }
                        rule
                    ]
                } ] @>

        [<Fact>]
        let ``do nothing if rule already exists`` () =
            let rule = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
            let config = { defaultConfig with Rules = [ rule ] }

            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    LoadConfig = fun () -> Ok config
                    UpdateConfig = calls.Add >> Ok
            }

            let result = Rules.add infra rule

            test <@ result = Ok () @>
            test <@ calls |> Seq.isEmpty @>

        [<Fact>]
        let ``do nothing if adding 'norule'`` () =
            let rule = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = SyncRules.NoRule }
            let config = { defaultConfig with Rules = [] }

            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    LoadConfig = fun () -> Ok config
                    UpdateConfig = calls.Add >> Ok
            }

            let result = Rules.add infra rule

            test <@ result = Ok () @>
            test <@ calls |> Seq.isEmpty @>

        let ``ask for rules conflict - test cases`` () : obj[] list = [
            [| SyncRules.Exclude; SyncRules.Include; SyncRules.Exclude |]
            [| SyncRules.Exclude; SyncRules.Include; SyncRules.Include |]
            [| SyncRules.Include; SyncRules.Exclude; SyncRules.Include |]
            [| SyncRules.Include; SyncRules.Exclude; SyncRules.Exclude |]
        ]

        [<Theory; MemberData(nameof ``ask for rules conflict - test cases``)>]
        let ``ask for rules conflict`` (existingRule: SyncRules) (newRule: SyncRules) (ruleChoseOnConflict: SyncRules) =
            let existingRule = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = existingRule }
            let newRule = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = newRule }
            let ruleChoseOnConflict = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = ruleChoseOnConflict }

            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    LoadConfig = fun () -> Ok { defaultConfig with Rules = [ existingRule ] }
                    SolveRuleConflict = fun rule1 rule2 ->
                        test <@ rule1 = existingRule @>
                        test <@ rule2 = newRule @>
                        Ok ruleChoseOnConflict
                    UpdateConfig = calls.Add >> Ok
            }

            let result = Rules.add infra newRule

            test <@ result = Ok () @>
            test <@ calls |> Seq.toList = [ { defaultConfig with Rules = [ ruleChoseOnConflict ] } ] @>

        [<Fact>]
        let ``remove rule conflict resolution said norule`` () =
            let existingRule = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
            let newRule = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = SyncRules.NoRule }
            let ruleChoseOnConflict = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = SyncRules.NoRule }

            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    LoadConfig = fun () -> Ok { defaultConfig with Rules = [ existingRule ] }
                    SolveRuleConflict = fun rule1 rule2 ->
                        test <@ rule1 = existingRule @>
                        test <@ rule2 = newRule @>
                        Ok ruleChoseOnConflict
                    UpdateConfig = calls.Add >> Ok
            }

            let result = Rules.add infra newRule

            test <@ result = Ok () @>
            test <@ calls |> Seq.toList = [ { defaultConfig with Rules = [] } ] @>
