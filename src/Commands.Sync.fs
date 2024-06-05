module SyncBackup.Commands.Sync

open SyncBackup
open SyncBackup.Domain.Dsl
open SyncBackup.Domain.Sync

type Infra = {
    LoadSource: LoadInfra
    LoadBackup: LoadInfra
    SaveSyncInstructionsFile: SyncInstruction list -> Result<unit, string>
    OpenSyncInstructionsForUserEdition: unit -> Result<unit, string>
    AreInstructionsAccepted: unit -> Result<bool, string>
    SubmitSyncInstructions: Alias list -> SyncInstruction list -> Result<unit, string>
}
and LoadInfra = {
    LoadConfig: unit -> Result<RepositoryConfig, string>
    LoadElements: unit -> Result<RelativePath list, string>
}

let sync (infra: Infra) =
    result {
        let! sourceConfig = infra.LoadSource.LoadConfig ()
        let! backupConfig = infra.LoadBackup.LoadConfig ()

        do! if sourceConfig.Type = RepositoryType.Source
            then Ok ()
            else Error "Invalid repository type."

        do! if backupConfig.Type = RepositoryType.Backup
            then Ok ()
            else Error "Invalid repository type."

        let! sourceElements = infra.LoadSource.LoadElements ()
        let! backupElements = infra.LoadBackup.LoadElements ()

        let! instructions = synchronize sourceElements sourceConfig.Rules backupElements backupConfig.Rules
        do! infra.SaveSyncInstructionsFile instructions
        do! infra.OpenSyncInstructionsForUserEdition ()

        match! infra.AreInstructionsAccepted () with
        | true ->
            do! infra.SubmitSyncInstructions sourceConfig.Aliases instructions
            return "Synchronization completed!"
        | false -> return "Synchronization aborted!"
    }
