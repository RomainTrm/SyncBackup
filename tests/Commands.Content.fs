module SyncBackup.Tests.Commands.Content

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open SyncBackup.Domain
open SyncBackup.Commands.Content
open SyncBackup.Tests.Properties.CustomGenerators

module ``scanRepositoryContent should`` =
    let defaultInfra = {
        LoadConfig = fun _ -> failwith "not implemented"
        LoadFiles = fun _ -> failwith "not implemented"
        SaveTempContent = fun _ -> failwith "not implemented"
        OpenForUserEdition = fun _ -> failwith "not implemented"
        ReadTempContent = fun _ -> failwith "not implemented"
        SaveTrackFile = fun _ -> failwith "not implemented"
        SaveRules = fun _ -> failwith "not implemented"
    }
    let defaultConfig : Dsl.RepositoryConfig = {
        IsSourceRepository = true
        Aliases = []
        Rules = []
    }

    [<Property(Arbitrary = [| typeof<NonWhiteSpaceStringGenerator> |])>]
    let ``retrieve content for repository, save it then open editor, then save track file and rules`` aliases content (contentEdited: Dsl.Rule list) =
        content <> [] ==> lazy
        let contentEdited = contentEdited |> List.distinctBy _.Path
        let calls = System.Collections.Generic.List<_> ()
        let infra = {
            LoadConfig = fun () -> Ok { defaultConfig with Aliases = aliases }
            LoadFiles = fun a ->
                test <@ a = aliases @>
                content
            SaveTempContent = fun _ ->
                calls.Add "save temp file" |> Ok
            OpenForUserEdition = fun () -> calls.Add "open editor" |> Ok
            ReadTempContent = fun () -> Ok contentEdited
            SaveTrackFile = fun c ->
                let expected = contentEdited |> List.map _.Path
                test <@ c = expected @>
                calls.Add "save track file" |> Ok
            SaveRules = fun rules ->
                let expected = contentEdited |> List.filter (fun rule -> rule.SyncRule <> Dsl.NoRule)
                test <@ rules = expected @>
                calls.Add "save rules" |> Ok
        }

        let result = scanRepositoryContent infra ()
        test <@ result = Ok () @>
        test <@ calls |> Seq.toList = ["save temp file"; "open editor"; "save track file"; "save rules"] @>

    [<Fact>]
    let ``build apply existing rules to scanned content`` () =
        let savedRules = System.Collections.Generic.List<_> ()
        let infra = {
            defaultInfra with
                LoadConfig = fun () -> Ok {
                    defaultConfig with
                        Rules = [
                            { Path = Dsl.Source "path1"; SyncRule = Dsl.Include }
                            { Path = Dsl.Source "path2"; SyncRule = Dsl.Exclude }
                        ]
                }
                LoadFiles = fun _ -> [
                    Dsl.File { Name = "file1"; RelativePath = Dsl.Source "path1" }
                    Dsl.File { Name = "file2"; RelativePath = Dsl.Source "path2" }
                    Dsl.File { Name = "file3"; RelativePath = Dsl.Source "path3" }
                ]
                SaveTempContent = fun rules ->
                    savedRules.AddRange rules
                    Error "I don't want to setup the rest of the infra"
        }

        let _ = scanRepositoryContent infra ()

        let expected: Dsl.Rule list = [
            { Path = Dsl.Source "path1"; SyncRule = Dsl.Include }
            { Path = Dsl.Source "path2"; SyncRule = Dsl.Exclude }
            { Path = Dsl.Source "path3"; SyncRule = Dsl.NoRule }
        ]
        test <@ savedRules |> Seq.toList = expected @>

    [<Property>]
    let ``return default message when empty`` aliases =
        let infra = {
            defaultInfra with
                LoadConfig = fun () -> Ok { defaultConfig with Aliases = aliases }
                LoadFiles = fun a ->
                    test <@ a = aliases @>
                    []
        }

        let result = scanRepositoryContent infra ()
        test <@ result = Error "Repository is empty." @>
