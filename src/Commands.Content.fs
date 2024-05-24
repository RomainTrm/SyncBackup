module SyncBackup.Commands.Content

open Microsoft.FSharp.Core
open SyncBackup.Domain.Dsl

type Infra = {
    LoadAliases: unit -> Result<Alias list, string>
    LoadFiles: Alias list -> Content list
    SaveTempContent: Content list -> Result<unit, string>
    OpenForUserEdition: unit -> Result<unit, string>
    ReadTempContent: unit -> Result<Content list, string>
    SaveTrackFile: Content list -> Result<unit, string>
}

let scanRepositoryContent (infra: Infra) () =
    infra.LoadAliases ()
    |> Result.map infra.LoadFiles
    |> Result.bind (function
        | [] -> Error "Repository is empty."
        | content -> Ok content
    )
    |> Result.bind infra.SaveTempContent
    |> Result.bind infra.OpenForUserEdition
    |> Result.bind infra.ReadTempContent
    |> Result.bind infra.SaveTrackFile
