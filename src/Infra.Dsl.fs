module SyncBackup.Infra.Dsl

open System.IO

let [<Literal>] NewLine = "\r\n"
let [<Literal>] ConfigDirectory = ".syncbackup"
let [<Literal>] ConfigFile = "CONFIG"
let [<Literal>] ScanFile = "TMP-SCAN"

let getFullConfigFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, ConfigFile)
let getScanFileFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, ScanFile)
