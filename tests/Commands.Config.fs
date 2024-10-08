module SyncBackup.Tests.Commands.Config

open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Commands.Config

let defaultInfra : Infra = {
    InitConfig = fun _ -> failwith "not implemented"
    LoadConfig = fun _ -> failwith "not implemented"
    CheckPathExists = fun _ -> failwith "not implemented"
    BuildRelativePath = fun _ _ -> failwith "not implemented"
    UpdateConfig = fun _ -> failwith "not implemented"
    SolveRuleConflict = fun _ _ -> failwith "not implemented"
    SolveContentType = fun _ -> failwith "not implemented"
    LoadTrackFile = fun _ -> failwith "not implemented"
    OpenRulesFile = fun _ -> failwith "not implemented"
    ReadRulesFile = fun _ -> failwith "not implemented"
    SaveRulesFile = fun _ _ -> failwith "not implemented"
}

let defaultConfig : RepositoryConfig = {
    Version = RepositoryConfigVersion
    Type = RepositoryType.Source
    Aliases = []
    Rules = []
}

module ``Init should`` =
    [<Fact>]
    let ``ìnit source repository`` () =
        let calls = System.Collections.Generic.List<_> ()
        let infra = {
            defaultInfra with
                InitConfig = calls.Add >> Ok
        }

        let result = Init.source infra

        test <@ result = Ok () @>
        let expectedConfig: RepositoryConfig = {
            Version = RepositoryConfigVersion
            Type = RepositoryType.Source
            Aliases = []
            Rules = []
        }
        test <@ calls |> Seq.toList = [ expectedConfig ] @>

    [<Fact>]
    let ``ìnit backup repository`` () =
        let calls = System.Collections.Generic.List<_> ()
        let infra = {
            defaultInfra with
                InitConfig = calls.Add >> Ok
        }

        let result = Init.backup infra

        test <@ result = Ok () @>
        let expectedConfig: RepositoryConfig = {
            Version = RepositoryConfigVersion
            Type = RepositoryType.Backup
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
                        Ok { defaultConfig with Type = RepositoryType.Backup }
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
            let rule = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
            let infra = {
                defaultInfra with
                    LoadConfig = fun () ->
                        Ok { defaultConfig with Rules = [] }
                    BuildRelativePath = fun _ unverifiedPath ->
                        test <@ unverifiedPath = rule.Path.Value @>
                        Ok rule.Path
                    UpdateConfig = calls.Add >> Ok
            }

            let rule = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
            let result = Rules.add infra rule.SyncRule rule.Path.Value

            test <@ result = Ok () @>
            test <@ calls |> Seq.toList = [ { defaultConfig with Rules = [ rule ] } ] @>

        [<Fact>]
        let ``add rule to config with existing rules`` () =
            let calls = System.Collections.Generic.List<_> ()
            let rule = { Path = { Type = Source; Value = "directory path"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
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
                    BuildRelativePath = fun _ unverifiedPath ->
                        test <@ unverifiedPath = rule.Path.Value @>
                        Ok rule.Path
                    UpdateConfig = calls.Add >> Ok
            }

            let result = Rules.add infra rule.SyncRule rule.Path.Value

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
                    BuildRelativePath = fun _ unverifiedPath ->
                        test <@ unverifiedPath = rule.Path.Value @>
                        Ok rule.Path
                    UpdateConfig = calls.Add >> Ok
            }

            let result = Rules.add infra rule.SyncRule rule.Path.Value

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
                    BuildRelativePath = fun _ unverifiedPath ->
                        test <@ unverifiedPath = rule.Path.Value @>
                        Ok rule.Path
                    UpdateConfig = calls.Add >> Ok
            }

            let result = Rules.add infra rule.SyncRule rule.Path.Value

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
                    BuildRelativePath = fun _ unverifiedPath ->
                        test <@ unverifiedPath = newRule.Path.Value @>
                        Ok newRule.Path
                    SolveRuleConflict = fun rule1 rule2 ->
                        test <@ rule1 = existingRule @>
                        test <@ rule2 = newRule @>
                        Ok ruleChoseOnConflict
                    UpdateConfig = calls.Add >> Ok
            }

            let result = Rules.add infra newRule.SyncRule newRule.Path.Value

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
                    BuildRelativePath = fun _ unverifiedPath ->
                        test <@ unverifiedPath = newRule.Path.Value @>
                        Ok newRule.Path
                    SolveRuleConflict = fun rule1 rule2 ->
                        test <@ rule1 = existingRule @>
                        test <@ rule2 = newRule @>
                        Ok ruleChoseOnConflict
                    UpdateConfig = calls.Add >> Ok
            }

            let result = Rules.add infra newRule.SyncRule newRule.Path.Value

            test <@ result = Ok () @>
            test <@ calls |> Seq.toList = [ { defaultConfig with Rules = [] } ] @>

        let ``return error if rule is not available for this repository type - test cases`` () : obj[] list = [
            [| RepositoryType.Source; SyncRules.NotDelete |]
            [| RepositoryType.Source; SyncRules.NotSave |]
            [| RepositoryType.Source; SyncRules.AlwaysReplace |]
            [| RepositoryType.Backup; SyncRules.Include |]
            [| RepositoryType.Backup; SyncRules.Exclude |]
        ]

        [<Theory; MemberData(nameof ``return error if rule is not available for this repository type - test cases``)>]
        let ``return error if rule is not available for this repository type`` repositoryType syncRule =
            let infra = {
                defaultInfra with
                    LoadConfig = fun () -> Ok { defaultConfig with Type = repositoryType }
            }

            let result = Rules.add infra syncRule "path"
            test <@ Result.isError result @>

        let ``ask for content type on 'ignore' rule - test cases`` () : obj[] list = [
            [| ContentType.Directory |]
            [| ContentType.File |]
        ]

        [<Theory; MemberData(nameof ``ask for content type on 'ignore' rule - test cases``)>]
        let ``ask for content type on 'ignore' rule`` (contentType: ContentType) =
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    LoadConfig = fun () -> Ok { defaultConfig with Type = RepositoryType.Backup }
                    SolveContentType = fun () -> Ok contentType
                    UpdateConfig = calls.Add >> Ok
            }

            let result = Rules.add infra SyncRules.NotSave "path"

            test <@ result = Ok () @>
            let expectedConfig = {
                defaultConfig with
                    Type = RepositoryType.Backup
                    Rules = [ { Path = { Type = Source; Value = "path"; ContentType = contentType }; SyncRule = SyncRules.NotSave } ]
            }
            test <@ calls |> Seq.toList = [ expectedConfig ] @>

    module ``Edit should`` =
        let d1 = { Value = "d1"; Type = Source; ContentType = Directory }
        let d2 = { Value = "d2"; Type = Source; ContentType = Directory }
        let d1s1 = { Value = "d1/s1"; Type = Source; ContentType = Directory }
        let d1s2 = { Value = "d1/s2"; Type = Source; ContentType = Directory }

        [<Fact>]
        let ``load tracked elements and existing rules, then save to file for user edition`` () =
            let saveRulesCalls = System.Collections.Generic.List<_> ()
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    LoadConfig = fun () -> Ok {
                        defaultConfig with
                            Rules = [
                                { Path = d2; SyncRule = Exclude }
                            ]
                        }
                    LoadTrackFile = fun () -> Ok [
                        { Path = d1; LastWriteTime = None }
                        { Path = d2; LastWriteTime = None }
                        { Path = d1s1; LastWriteTime = None }
                        { Path = d1s2; LastWriteTime = None }
                    ]
                    SaveRulesFile = fun repoType rules ->
                        saveRulesCalls.Add(repoType, rules)
                        Ok ()
                    OpenRulesFile = Ok
                    ReadRulesFile = fun () -> Ok [
                        { Path = d1; SyncRule = NoRule }
                        { Path = d1s1; SyncRule = Exclude }
                        { Path = d1s2; SyncRule = NoRule }
                        { Path = d2; SyncRule = Exclude }
                    ]
                    UpdateConfig = calls.Add >> Ok
            }

            let result = Rules.editRules infra
            test <@ result = Ok () @>

            let expectedSaveRulesFile = RepositoryType.Source, [
                { Path = d1; SyncRule = NoRule }
                { Path = d1s1; SyncRule = NoRule }
                { Path = d1s2; SyncRule = NoRule }
                { Path = d2; SyncRule = Exclude }
            ]
            test <@ saveRulesCalls |> Seq.toList = [expectedSaveRulesFile] @>

            let expectedSavedRules = [
                { Path = d2; SyncRule = Exclude }
                { Path = d1s1; SyncRule = Exclude }
            ]
            test <@ calls |> Seq.toList = [{ defaultConfig with Rules = expectedSavedRules }] @>

        [<Fact>]
        let ``return error if user set an invalid rule for the repository`` () =
            let invalidRule = SyncRules.AlwaysReplace
            let calls = System.Collections.Generic.List<_> ()
            let infra = {
                defaultInfra with
                    LoadConfig = fun () -> Ok {
                        defaultConfig with
                            Rules = [
                                { Path = d2; SyncRule = Exclude }
                            ]
                        }
                    LoadTrackFile = fun () -> Ok [
                        { Path = d1; LastWriteTime = None }
                        { Path = d2; LastWriteTime = None }
                        { Path = d1s1; LastWriteTime = None }
                        { Path = d1s2; LastWriteTime = None }
                    ]
                    SaveRulesFile = fun _ _ -> Ok ()
                    OpenRulesFile = Ok
                    ReadRulesFile = fun () -> Ok [
                        { Path = d1; SyncRule = NoRule }
                        { Path = d1s1; SyncRule = Exclude }
                        { Path = d1s2; SyncRule = invalidRule }
                        { Path = d2; SyncRule = Exclude }
                    ]
                    UpdateConfig = calls.Add >> Ok
            }

            let result = Rules.editRules infra
            test <@ result = Error $"The rule \"{SyncRules.getValue invalidRule}\" can't be applied to this repository type." @>
            test <@ calls |> Seq.isEmpty @>
