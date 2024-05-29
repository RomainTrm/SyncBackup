module SyncBackup.Cli.Factory

let configCommandInfra logger currentDirectory : SyncBackup.Commands.Config.Infra = {
    InitConfig = SyncBackup.Infra.Config.init currentDirectory
    LoadConfig = fun () -> SyncBackup.Infra.Config.load currentDirectory
    CheckPathExists = SyncBackup.Infra.Core.checkPathExists
    BuildRelativePath = SyncBackup.Infra.Core.buildRelativePath currentDirectory
    UpdateConfig = SyncBackup.Infra.Config.update currentDirectory
    SolveRuleConflict = Rules.solveConflict logger
}

let configQueryInfra currentDirectory : SyncBackup.Queries.Config.Infra = {
    LoadConfig = fun () -> SyncBackup.Infra.Config.load currentDirectory
}

let contentCommandInfra currentDirectory : SyncBackup.Commands.Scan.Infra = {
    ScanRepositoryContent = SyncBackup.Infra.Content.Scan.run currentDirectory
    LoadConfig = fun () -> SyncBackup.Infra.Config.load currentDirectory
    LoadTrackFile = fun () -> SyncBackup.Infra.Content.TrackFile.load currentDirectory
    SaveScanFileContent = SyncBackup.Infra.Content.ScanFile.writeFile currentDirectory
    OpenScanFileForUserEdition = fun () ->
        SyncBackup.Infra.Dsl.getScanFileFilePath currentDirectory
        |> SyncBackup.Infra.Editor.VsCode.runEditor
    ReadScanFileContent = fun () -> SyncBackup.Infra.Content.ScanFile.readFile currentDirectory
    SaveTrackFile = SyncBackup.Infra.Content.TrackFile.save currentDirectory
    SaveRules = fun rules ->
        SyncBackup.Infra.Config.load currentDirectory
        |> Result.map (fun config -> { config with Rules = rules })
        |> Result.bind (SyncBackup.Infra.Config.update currentDirectory)
}

let syncCommandInfra sourceDirectory backupDirectory : SyncBackup.Commands.Sync.Infra = {
    LoadSource = {
        LoadRules = fun () -> SyncBackup.Infra.Config.load sourceDirectory |> Result.map _.Rules
        LoadElements = fun () -> SyncBackup.Infra.Content.TrackFile.load sourceDirectory
    }
    LoadBackup = {
        LoadRules = fun () -> SyncBackup.Infra.Config.load backupDirectory |> Result.map _.Rules
        LoadElements = fun () -> SyncBackup.Infra.Content.TrackFile.load backupDirectory
    }
    SubmitSyncInstructions = fun instructions -> failwith "not implemented"
}
