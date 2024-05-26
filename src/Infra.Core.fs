module SyncBackup.Infra.Core

open SyncBackup.Domain.Dsl
open System.IO

let checkPathExists (path: DirectoryPath) =
    if Directory.Exists path
    then Ok ()
    else Error "The specified directory path doesn't exist"
