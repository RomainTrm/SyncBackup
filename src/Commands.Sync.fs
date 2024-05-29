﻿module SyncBackup.Commands.Sync

open SyncBackup
open SyncBackup.Domain
open SyncBackup.Domain.Dsl
open SyncBackup.Domain.Sync

type Infra = {
    LoadSource: LoadInfra
    LoadBackup: LoadInfra
    SubmitSyncInstructions: SyncInstruction list -> Result<unit, string>
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

        let! sourceRules =
            infra.LoadSource.LoadElements ()
            |> Result.map (Rules.buildRulesForSyncing sourceConfig.Rules)

        let! backupRules =
            infra.LoadBackup.LoadElements ()
            |> Result.map (Rules.buildRulesForSyncing backupConfig.Rules)

        let! instructions = synchronize sourceRules backupRules
        do! infra.SubmitSyncInstructions instructions
    }