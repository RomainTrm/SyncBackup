module SyncBackup.Infra.Dsl

open System.IO

let [<Literal>] ConfigDirectory = ".syncbackup"
let [<Literal>] ConfigFile = "CONFIG"

let configFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, ConfigFile)
