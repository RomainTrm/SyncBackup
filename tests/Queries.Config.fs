module SyncBackup.Tests.Queries.Config

open SyncBackup.Domain.Dsl
open SyncBackup.Queries.Config
open Xunit
open Swensen.Unquote

module Alias =
    module ``list should`` =
        let repositoryConfig : RepositoryConfig = {
            IsSourceRepository = true
            Aliases = []
            Rules = []
        }

        [<Fact>]
        let ``return error if load fail`` () =
            let infra = {
                LoadConfig = fun () -> Error "some error"
            }
            let result = Alias.list infra
            test <@ result = Error "some error" @>

        [<Fact>]
        let ``return message when no alias`` () =
            let infra = {
                LoadConfig = fun () -> Ok {
                    repositoryConfig with Aliases = []
                }
            }
            let result = Alias.list infra
            test <@ result = Ok ["No alias configured"] @>

        [<Fact>]
        let ``return formatted aliases`` () =
            let infra = {
                LoadConfig = fun () -> Ok {
                    repositoryConfig with
                        Aliases = [
                            { Name = "Alias 1"; Path = @"C:\path\subpath1" }
                            { Name = "Alias 2"; Path = @"C:\path\subpath2" }
                        ]
                }
            }
            let result = Alias.list infra
            let expectedLines = [
                @"Alias 1 => C:\path\subpath1"
                @"Alias 2 => C:\path\subpath2"
            ]
            test <@ result = Ok expectedLines @>

        [<Fact>]
        let ``sort aliases by name`` () =
            let infra = {
                LoadConfig = fun () -> Ok {
                    repositoryConfig with
                        Aliases = [
                            { Name = "Alias 2"; Path = @"C:\path\subpath2" }
                            { Name = "Alias 3"; Path = @"C:\path\subpath3" }
                            { Name = "Alias 1"; Path = @"C:\path\subpath1" }
                        ]
                }
            }
            let result = Alias.list infra
            let expectedLines = [
                @"Alias 1 => C:\path\subpath1"
                @"Alias 2 => C:\path\subpath2"
                @"Alias 3 => C:\path\subpath3"
            ]
            test <@ result = Ok expectedLines @>

module Rules =
    module ``list should`` =
        let repositoryConfig : RepositoryConfig = {
            IsSourceRepository = true
            Aliases = []
            Rules = []
        }

        [<Fact>]
        let ``return error if load fail`` () =
            let infra = {
                LoadConfig = fun () -> Error "some error"
            }
            let result = Rules.list infra
            test <@ result = Error "some error" @>

        [<Fact>]
        let ``return message when no alias`` () =
            let infra = {
                LoadConfig = fun () -> Ok {
                    repositoryConfig with Rules = []
                }
            }
            let result = Rules.list infra
            test <@ result = Ok ["No rule configured"] @>

        [<Fact>]
        let ``return formatted aliases`` () =
            let infra = {
                LoadConfig = fun () -> Ok {
                    repositoryConfig with
                        Rules = [
                            { Path = { Type = Source; Value = @"C:\path\subpath1"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
                            { Path = { Type = Alias; Value = @"C:\path\subpath2"; ContentType = Directory }; SyncRule = SyncRules.Include }
                        ]
                }
            }
            let result = Rules.list infra
            let expectedLines = [
                "exclude \"C:\\path\\subpath1\""
                "include \"*C:\\path\\subpath2\""
            ]
            test <@ result = Ok expectedLines @>

        [<Fact>]
        let ``sort rules by path`` () =
            let infra = {
                LoadConfig = fun () -> Ok {
                    repositoryConfig with
                        Rules = [
                            { Path = { Type = Source; Value = @"C:\path\subpath3"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
                            { Path = { Type = Source; Value = @"C:\path\subpath1"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
                            { Path = { Type = Source; Value = @"C:\path\subpath2"; ContentType = Directory }; SyncRule = SyncRules.Exclude }
                        ]
                }
            }
            let result = Rules.list infra
            let expectedLines = [
                "exclude \"C:\\path\\subpath1\""
                "exclude \"C:\\path\\subpath2\""
                "exclude \"C:\\path\\subpath3\""
            ]
            test <@ result = Ok expectedLines @>
