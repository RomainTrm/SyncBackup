module SyncBackup.Commands.Content

open Microsoft.FSharp.Core
open SyncBackup.Domain.Dsl

type Infra = {
    LoadAliases: unit -> Result<Alias list, string>
    LoadFiles: Alias list -> Content list
}

let rec private printContent = function
    | Directory { RelativePath = Source path; Content = content } ->
        [
            $"{path} (directory)"
            yield! content |> List.collect printContent
        ]
    | Directory { RelativePath = Alias path; Content = content } ->
        [
            $"{path} (directory, alias)"
            yield! content |> List.collect printContent
        ]
    | File { RelativePath = Source path } -> [ $"{path} (file)" ]
    | File { RelativePath = Alias path } -> [ $"{path} (file, alias)" ]

let scanRepositoryContent (infra: Infra) () =
    infra.LoadAliases ()
    |> Result.map infra.LoadFiles
    |> Result.map (List.collect printContent)
