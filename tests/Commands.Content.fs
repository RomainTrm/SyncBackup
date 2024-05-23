module SyncBackup.Tests.Commands.Content

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open SyncBackup.Commands.Content

module ``scanRepositoryContent should`` =
    let defaultInfra = {
        LoadAliases = fun _ -> failwith "not implemented"
        LoadFiles = fun _ -> failwith "not implemented"
        SaveTempContent = fun _ -> failwith "not implemented"
        OpenForUserEdition = fun _ -> failwith "not implemented"
    }

    [<Property>]
    let ``retrieve content for repository, save it then open editor`` aliases content =
        content <> [] ==> lazy
        let calls = System.Collections.Generic.List<_> ()
        let infra = {
            LoadAliases = fun () -> Ok aliases
            LoadFiles = fun a ->
                test <@ a = aliases @>
                content
            SaveTempContent = fun c ->
                test <@ c = content @>
                calls.Add "save temp file" |> Ok
            OpenForUserEdition = fun () -> calls.Add "open editor" |> Ok
        }

        let result = scanRepositoryContent infra ()
        test <@ result = Ok () @>
        test <@ calls |> Seq.toList = ["save temp file"; "open editor"] @>

    [<Property>]
    let ``return default message when empty`` aliases =
        let infra = {
            defaultInfra with
                LoadAliases = fun () -> Ok aliases
                LoadFiles = fun a ->
                    test <@ a = aliases @>
                    []
        }

        let result = scanRepositoryContent infra ()
        test <@ result = Error "Repository is empty." @>
