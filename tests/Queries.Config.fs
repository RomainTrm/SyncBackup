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
