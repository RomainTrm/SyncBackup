module SyncBackup.Queries.Config

open SyncBackup.Domain.Dsl

type Infra = {
    LoadConfig: unit -> Result<RepositoryConfig, string>
}

module Alias =
    let list (infra: Infra) =
        infra.LoadConfig ()
        |> Result.map (function
            | { Aliases = [] }  -> ["No alias configured"]
            | { Aliases = aliases } -> aliases |> List.map (fun alias -> $"{alias.Name} => {alias.Path}")
        )
