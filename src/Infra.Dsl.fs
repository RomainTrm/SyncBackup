﻿module SyncBackup.Infra.Dsl

open System.IO

let [<Literal>] NewLine = "\r\n"
let [<Literal>] ConfigDirectory = ".syncbackup"
let [<Literal>] ConfigFile = "CONFIG"
let [<Literal>] ScanFile = "TMP-SCAN"
let [<Literal>] InstructionsFile = "TMP-INSTRUCTIONS"
let [<Literal>] RulesFile = "TMP-RULES"
let [<Literal>] TrackFile = "TRACK"
let [<Literal>] ErrorLogFilePrefix = "log"

let isInRepository repositoryPath = Path.Combine(repositoryPath, ConfigDirectory) |> Directory.Exists
let getFullConfigFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, ConfigFile)
let getScanFileFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, ScanFile)
let getTrackFileFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, TrackFile)
let getSyncInstructionsFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, InstructionsFile)
let getRulesEditionFilePath repositoryPath = Path.Combine(repositoryPath, ConfigDirectory, RulesFile)
let getErrorLogFilePath repositoryPath (now: System.DateTime) = Path.Combine(repositoryPath, ConfigDirectory, $"{ErrorLogFilePrefix}-{now:``yyyy-MM-dd-HH-mm-ss``}")
