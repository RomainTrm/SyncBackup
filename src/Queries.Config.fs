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
            | { Aliases = aliases } ->
                aliases
                |> List.sortBy _.Name
                |> List.map (fun alias -> $"{alias.Name} => {alias.Path}")
        )

module Rules =
    let list (infra: Infra) =
        infra.LoadConfig ()
        |> Result.map (function
            | { Rules = [] }  -> ["No rule configured"]
            | { Rules = rules } ->
                rules
                |> List.sortBy (fun rule ->
                    match rule.Path with
                    | Source path -> path
                    | Alias path -> path
                )
                |> List.map (fun rule ->
                    let path =
                        match rule.Path with
                        | Source path -> path
                        | Alias path -> path
                    $"{SyncRules.getValue rule.SyncRule} \"{path}\""
                )
        )
