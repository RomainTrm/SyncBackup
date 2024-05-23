module SyncBackup.Tests.Infra.TestHelpers

open System
open System.IO
open SyncBackup.Domain.Dsl

let currentDirectory = Environment.CurrentDirectory
let testDirectoryPath uniqueTestDirectory = Path.Combine(currentDirectory, uniqueTestDirectory)
let cleanupTests uniqueTestDirectory =
    let testDirectoryPath = testDirectoryPath uniqueTestDirectory
    if Directory.Exists testDirectoryPath
    then Directory.Delete (testDirectoryPath, true)

let createDirectory (args: string[]) =
    let path = Path.Combine args
    if (not<<Directory.Exists) path
    then Directory.CreateDirectory path |> ignore<DirectoryInfo>

let createFile ([<ParamArray>] args: string[]) =
    let path = Path.Combine args
    if (not<<File.Exists) path
    then File.WriteAllText (path, "")
