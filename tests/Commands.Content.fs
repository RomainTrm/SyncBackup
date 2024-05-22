module SyncBackup.Tests.Commands.Content

open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Commands.Content

module ``scanRepositoryContent should`` =
    let aliases : Alias list = [
        { Name = "alias1"; Path = "path1" }
        { Name = "alias2"; Path = "path2" }
    ]

    let content = [
        Directory { Name = "dir"; RelativePath = Source "path1"; Content = [] }
        File { Name = "file"; RelativePath = Source "path2" }
        Directory { Name = "dir2"; RelativePath = Source "path3"; Content = [
            File { Name = "file"; RelativePath = Source "path3\\path4" }
            Directory { Name = "dir"; RelativePath = Source "path3\\path5"; Content = [
                File { Name = "file"; RelativePath = Source "path3\\path5\\path6" }
            ] }
        ] }
        File { Name = "file"; RelativePath = Alias "path7" }
    ]

    [<Fact>]
    let ``retrieve content for repository and format it`` () =
        let infra = {
            LoadAliases = fun () -> Ok aliases
            LoadFiles = fun a ->
                test <@ a = aliases @>
                content
        }

        let result = scanRepositoryContent infra ()
        let expected = [
            "path1 (directory)"
            "path2 (file)"
            "path3 (directory)"
            "path3\\path4 (file)"
            "path3\\path5 (directory)"
            "path3\\path5\\path6 (file)"
            "path7 (file, alias)"
        ]
        test <@ result = Ok expected @>


    [<Fact>]
    let ``return default message when empty`` () =
        let infra = {
            LoadAliases = fun () -> Ok aliases
            LoadFiles = fun a ->
                test <@ a = aliases @>
                []
        }

        let result = scanRepositoryContent infra ()
        test <@ result = Ok ["Repository is empty."] @>
