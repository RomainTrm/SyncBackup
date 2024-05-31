module SyncBackup.Infra.Sync

open System
open System.IO
open Microsoft.FSharp.Core
open SyncBackup.Domain.Dsl
open SyncBackup.Domain.Sync

module InstructionsFile =
    let private buildFileContent sourceDirectory backupDirectory instructions =
        let fileLines = [
            "# Synchronizing from:"
            $"#     {sourceDirectory}"
            "# to:"
            $"#     {backupDirectory}"
            "# If you accept the following changes, uncomment the next line (remove #) and save:"
            "# Accept"
            ""
            yield! instructions |> List.map SyncInstruction.serialize
            if instructions = []
            then "# No change, your backup is up to date."
        ]
        String.Join (Dsl.NewLine, fileLines)

    let save (sourceDirectory: RepositoryPath) (backupDirectory: RepositoryPath) (instructions: SyncInstruction list) =
        let fileContent = buildFileContent sourceDirectory backupDirectory instructions
        let filePath = Dsl.getSyncInstructionsFilePath sourceDirectory
        File.WriteAllText(filePath, fileContent)
        |> Ok

    let areInstructionsAccepted (repositoryPath: RepositoryPath) =
        let filePath = Dsl.getSyncInstructionsFilePath repositoryPath
        if not (File.Exists filePath)
        then Error "Missing instructions."
        else
            File.ReadAllLines filePath
            |> Array.tryFind (fun line -> line.Contains "Accept")
            |> Option.map (fun line -> line.Contains "#")
            |> Option.map not
            |> Option.defaultValue false
            |> Ok

module Process =
    let run (sourceDirectory: RepositoryPath) (backupDirectory: RepositoryPath) (logger: string -> unit) (instructions: SyncInstruction list) =
        Ok ()
