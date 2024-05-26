module SyncBackup.Infra.Core

open SyncBackup.Domain.Dsl
open System.IO

let checkPathExists (path: DirectoryPath) =
    if Directory.Exists path
    then Ok ()
    else Error "The specified directory path doesn't exist"

let buildRelativePath (repositoryPath: RepositoryPath) (aliases: Alias list) (unverifiedPath: UnverifiedPath) =
    let pathParts = unverifiedPath.Split(Path.DirectorySeparatorChar)
    let fullPath, pathType =
        aliases
        |> List.tryFind (fun alias -> alias.Name = pathParts[0])
        |> function
            | Some alias -> Path.Combine(alias.Path, System.String.Join(Path.DirectorySeparatorChar, Array.skip 1 pathParts)), Alias
            | None -> Path.Combine(repositoryPath, unverifiedPath), Source

    if Directory.Exists fullPath
    then Ok { Value = unverifiedPath; Type = pathType; ContentType = ContentType.Directory }
    elif File.Exists fullPath
    then Ok { Value = unverifiedPath; Type = pathType; ContentType = ContentType.File }
    else Error "The specified path doesn't exist"
