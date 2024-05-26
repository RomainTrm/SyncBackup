module SyncBackup.Cli.Factory

let configCommandInfra logger currentDirectory : SyncBackup.Commands.Config.Infra = {
    InitConfig = SyncBackup.Infra.Config.init currentDirectory
    LoadConfig = fun () -> SyncBackup.Infra.Config.load currentDirectory
    CheckPathExists = SyncBackup.Infra.Config.checkPathExists
    UpdateConfig = SyncBackup.Infra.Config.update currentDirectory
    SolveRuleConflict = Rules.solveConflict logger
}

let configQueryInfra currentDirectory : SyncBackup.Queries.Config.Infra = {
    LoadConfig = fun () -> SyncBackup.Infra.Config.load currentDirectory
}

let contentCommandInfra currentDirectory : SyncBackup.Commands.Content.Infra = {
    LoadRepositoryContent = SyncBackup.Infra.Content.Scan.run currentDirectory
    LoadConfig = fun () -> SyncBackup.Infra.Config.load currentDirectory
    SaveTempContent = SyncBackup.Infra.Content.ScanFile.writeFile currentDirectory
    OpenForUserEdition = fun () ->
        SyncBackup.Infra.Dsl.getScanFileFilePath currentDirectory
        |> SyncBackup.Infra.Editor.VsCode.runEditor
    ReadTempContent = SyncBackup.Infra.Content.ScanFile.readFile currentDirectory
    SaveTrackFile = SyncBackup.Infra.Content.TrackFile.save currentDirectory
    SaveRules = fun rules ->
        SyncBackup.Infra.Config.load currentDirectory
        |> Result.map (fun config -> { config with Rules = rules })
        |> Result.bind (SyncBackup.Infra.Config.update currentDirectory)
}
