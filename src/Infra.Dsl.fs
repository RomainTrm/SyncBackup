module SyncBackup.Infra.Dsl

open System.IO

let [<Literal>] NewLine = "\r\n"
let [<Literal>] ConfigDirectory = ".syncbackup"
let [<Literal>] ConfigFile = "CONFIG"
let [<Literal>] ScanFile = "TMP-SCAN"
let [<Literal>] InstructionsFile = "TMP-INSTRUCTIONS"
let [<Literal>] TrackFile = "TRACK"

let getFullConfigFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, ConfigFile)
let getScanFileFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, ScanFile)
let getTrackFileFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, TrackFile)
let getSyncInstructionsFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, InstructionsFile)
