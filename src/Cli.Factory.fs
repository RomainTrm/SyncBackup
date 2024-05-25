module SyncBackup.Cli.Factory

let configCommandInfra currentDirectory : SyncBackup.Commands.Config.Infra = {
    InitConfig = SyncBackup.Infra.Config.init currentDirectory
    LoadConfig = fun () -> SyncBackup.Infra.Config.load currentDirectory
    CheckPathExists = SyncBackup.Infra.Config.checkPathExists
    UpdateConfig = SyncBackup.Infra.Config.update currentDirectory
    SolveRuleConflict = fun rule1 rule2 -> failwith "not implemented"
}

let configQueryInfra currentDirectory : SyncBackup.Queries.Config.Infra = {
    LoadConfig = fun () -> SyncBackup.Infra.Config.load currentDirectory
}

let contentCommandInfra currentDirectory : SyncBackup.Commands.Content.Infra = {
    LoadFiles = SyncBackup.Infra.Content.Scan.run currentDirectory
    LoadAliases = fun () -> SyncBackup.Infra.Config.load currentDirectory |> Result.map _.Aliases
    SaveTempContent = SyncBackup.Infra.Content.ScanFile.writeFile currentDirectory
    OpenForUserEdition = fun () ->
        SyncBackup.Infra.Dsl.getScanFileFilePath currentDirectory
        |> SyncBackup.Infra.Editor.VsCode.runEditor
    ReadTempContent = SyncBackup.Infra.Content.ScanFile.readFile currentDirectory
    SaveTrackFile = SyncBackup.Infra.Content.TrackFile.save currentDirectory
}
