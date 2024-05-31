module SyncBackup.Infra.Sync

open System
open System.IO
open SyncBackup.Domain.Dsl
open SyncBackup.Domain.Sync

module InstructionsFile =
    let private buildFileContent sourceDirectory backupDirectory instructions =
        let fileLines = [
            "# Synchronizing from:"
            $"# {sourceDirectory}"
            "# to:"
            $"# {backupDirectory}"
            "# If you accept the following changes, uncomment next line (remove #):"
            "# accept"
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
