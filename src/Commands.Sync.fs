module SyncBackup.Commands.Sync

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
    LoadRules: unit -> Result<Rule list, string>
    LoadElements: unit -> Result<RelativePath list, string>
}

let sync (infra: Infra) =
    result {
        let! sourceRules =
            infra.LoadSource.LoadRules ()
            |> Result.bind (fun rules ->
                infra.LoadSource.LoadElements ()
                |> Result.map (Rules.buildRulesForSyncing rules)
            )

        let! backupRules =
            infra.LoadBackup.LoadRules ()
            |> Result.bind (fun rules ->
                infra.LoadBackup.LoadElements ()
                |> Result.map (Rules.buildRulesForSyncing rules)
            )

        let! instructions = synchronize sourceRules backupRules
        do! infra.SubmitSyncInstructions instructions
    }
