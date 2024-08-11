module SyncBackup.Tests.Commands.Sync

open System
open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Domain.Sync
open SyncBackup.Commands.Sync

let lastWriteTime = DateTime(2024, 08, 09, 20, 34, 30)

let d1 = { Path = { Type = Source; Value = "d1"; ContentType = Directory }; LastWriteTime = None }
let d2 = { Path = { Type = Source; Value = "d2"; ContentType = Directory }; LastWriteTime = None }
let d2f1 = { Path = { Type = Source; Value = "d2/f1"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d2f2 = { Path = { Type = Source; Value = "d2/f2"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d2f3 = { Path = { Type = Source; Value = "d2/f3"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d2f4 = { Path = { Type = Source; Value = "d2/f4"; ContentType = File }; LastWriteTime = Some lastWriteTime }

module ``sync should`` =
    let defaultInfra = {
        LoadSource = {
            LoadConfig = fun _ -> failwith "not implemented"
            LoadElements = fun _ -> failwith "not implemented"
        }
        LoadBackup = {
            LoadConfig = fun _ -> failwith "not implemented"
            LoadElements = fun _ -> failwith "not implemented"
        }
        SaveSyncInstructionsFile = fun _ -> failwith "not implemented"
        OpenSyncInstructionsForUserEdition = fun _ -> failwith "not implemented"
        AreInstructionsAccepted = fun _ -> failwith "not implemented"
        SubmitSyncInstructions = fun _ -> failwith "not implemented"
        UpdateTargetTrackFile = fun _ -> failwith "not implemented"
    }

    [<Fact>]
    let ``return error if designated source repository is a backup repository`` () =
        let result = sync {
            defaultInfra with
                LoadSource = {
                    LoadElements = fun () -> Ok []
                    LoadConfig = fun () -> Ok {
                        Version = RepositoryConfigVersion
                        Type = RepositoryType.Backup
                        Aliases = []
                        Rules = []
                    }
                }
                LoadBackup = {
                    LoadElements = fun () -> Ok []
                    LoadConfig = fun () -> Ok {
                        Version = RepositoryConfigVersion
                        Type = RepositoryType.Backup
                        Aliases = []
                        Rules = []
                    }
                }
        }
        test <@ Result.isError result @>

    [<Fact>]
    let ``return error if designated backup repository is a source repository`` () =
        let result = sync {
            defaultInfra with
                LoadSource = {
                    LoadElements = fun () -> Ok []
                    LoadConfig = fun () -> Ok {
                        Version = RepositoryConfigVersion
                        Type = RepositoryType.Source
                        Aliases = []
                        Rules = []
                    }
                }
                LoadBackup = {
                    LoadElements = fun () -> Ok []
                    LoadConfig = fun () -> Ok {
                        Version = RepositoryConfigVersion
                        Type = RepositoryType.Source
                        Aliases = []
                        Rules = []
                    }
                }
        }

        test <@ Result.isError result @>

    [<Fact>]
    let ``load data then submit instructions when accepted`` () =
        let instructionsSaved = System.Collections.Generic.List<_> ()
        let instructionsSubmitted = System.Collections.Generic.List<_> ()
        let infra = {
            LoadSource = {
                LoadElements = fun () -> Ok [d1; d2; d2f1; d2f2; d2f3]
                LoadConfig = fun () -> Ok {
                    Version = RepositoryConfigVersion
                    Type = RepositoryType.Source
                    Aliases = []
                    Rules = [{ Path = d1.Path; SyncRule = Exclude }]
                }
            }
            LoadBackup = {
                LoadElements = fun () -> Ok [d2; d2f3; d2f4]
                LoadConfig = fun () -> Ok {
                    Version = RepositoryConfigVersion
                    Type = RepositoryType.Backup
                    Aliases = []
                    Rules = [
                        { Path = d2f2.Path; SyncRule = NotSave }
                        { Path = d2f3.Path; SyncRule = AlwaysReplace }
                        { Path = d2f4.Path; SyncRule = NotDelete }
                    ]
                }
            }
            SaveSyncInstructionsFile = instructionsSaved.AddRange >> Ok
            OpenSyncInstructionsForUserEdition = fun () -> Ok ()
            AreInstructionsAccepted = fun () -> Ok true
            SubmitSyncInstructions = fun _ -> instructionsSubmitted.AddRange >> Ok
            UpdateTargetTrackFile = Ok
        }

        let result = sync infra
        test <@ result = Ok "Synchronization completed!" @>

        let expected = [
            Add d2f1.Path
            Replace d2f3.Path
        ]

        test <@ instructionsSaved |> Seq.toList = expected @>
        test <@ instructionsSubmitted |> Seq.toList = expected @>

    [<Fact>]
    let ``load data then not submit instructions when refused`` () =
        let instructionsSubmitted = System.Collections.Generic.List<_> ()
        let infra = {
            LoadSource = {
                LoadElements = fun () -> Ok [d1; d2; d2f1; d2f2; d2f3]
                LoadConfig = fun () -> Ok {
                    Version = RepositoryConfigVersion
                    Type = RepositoryType.Source
                    Aliases = []
                    Rules = [{ Path = d1.Path; SyncRule = Exclude }]
                }
            }
            LoadBackup = {
                LoadElements = fun () -> Ok [d2; d2f3; d2f4]
                LoadConfig = fun () -> Ok {
                    Version = RepositoryConfigVersion
                    Type = RepositoryType.Backup
                    Aliases = []
                    Rules = [
                        { Path = d2f2.Path; SyncRule = NotSave }
                        { Path = d2f3.Path; SyncRule = AlwaysReplace }
                        { Path = d2f4.Path; SyncRule = NotDelete }
                    ]
                }
            }
            SaveSyncInstructionsFile = ignore >> Ok
            OpenSyncInstructionsForUserEdition = fun () -> Ok ()
            AreInstructionsAccepted = fun () -> Ok false
            SubmitSyncInstructions = fun _ -> instructionsSubmitted.AddRange >> Ok
            UpdateTargetTrackFile = fun () -> Error "Should not be called"
        }

        let result = sync infra
        test <@ result = Ok "Synchronization aborted!" @>
        test <@ instructionsSubmitted |> Seq.isEmpty @>

module ``replicate should`` =
    let defaultInfra = {
        LoadSourceBackup = {
            LoadConfig = fun _ -> failwith "not implemented"
            LoadElements = fun _ -> failwith "not implemented"
        }
        LoadTargetBackup = {
            LoadConfig = fun _ -> failwith "not implemented"
            LoadElements = fun _ -> failwith "not implemented"
        }
        SaveSyncInstructionsFile = fun _ -> failwith "not implemented"
        OpenSyncInstructionsForUserEdition = fun _ -> failwith "not implemented"
        AreInstructionsAccepted = fun _ -> failwith "not implemented"
        SubmitSyncInstructions = fun _ -> failwith "not implemented"
        SaveTargetBackupRules = fun _ -> failwith "not implemented"
        UpdateTargetTrackFile = fun _ -> failwith "not implemented"
    }

    [<Fact>]
    let ``return error if designated source repository is a source repository`` () =
        let result = replicateBackup {
            defaultInfra with
                LoadSourceBackup = {
                    LoadElements = fun () -> Ok []
                    LoadConfig = fun () -> Ok {
                        Version = RepositoryConfigVersion
                        Type = RepositoryType.Source
                        Aliases = []
                        Rules = []
                    }
                }
                LoadTargetBackup = {
                    LoadElements = fun () -> Ok []
                    LoadConfig = fun () -> Ok {
                        Version = RepositoryConfigVersion
                        Type = RepositoryType.Backup
                        Aliases = []
                        Rules = []
                    }
                }
        }
        test <@ Result.isError result @>

    [<Fact>]
    let ``return error if designated target repository is a source repository`` () =
        let result = replicateBackup {
            defaultInfra with
                LoadSourceBackup = {
                    LoadElements = fun () -> Ok []
                    LoadConfig = fun () -> Ok {
                        Version = RepositoryConfigVersion
                        Type = RepositoryType.Backup
                        Aliases = []
                        Rules = []
                    }
                }
                LoadTargetBackup = {
                    LoadElements = fun () -> Ok []
                    LoadConfig = fun () -> Ok {
                        Version = RepositoryConfigVersion
                        Type = RepositoryType.Source
                        Aliases = []
                        Rules = []
                    }
                }
        }

        test <@ Result.isError result @>

    [<Fact>]
    let ``load data then submit instructions and save rules when accepted`` () =
        let sourceRules = [
            { Path = d2f2.Path; SyncRule = NotDelete }
            { Path = d2f3.Path; SyncRule = AlwaysReplace }
            { Path = d2f4.Path; SyncRule = NotSave }
        ]

        let instructionsSaved = System.Collections.Generic.List<_> ()
        let instructionsSubmitted = System.Collections.Generic.List<_> ()
        let rulesSaved = System.Collections.Generic.List<_> ()
        let infra = {
            LoadSourceBackup = {
                LoadElements = fun () -> Ok [d1; d2; d2f1; d2f2; d2f3]
                LoadConfig = fun () -> Ok {
                    Version = RepositoryConfigVersion
                    Type = RepositoryType.Backup
                    Aliases = []
                    Rules = sourceRules
                }
            }
            LoadTargetBackup = {
                LoadElements = fun () -> Ok [d2; d2f1]
                LoadConfig = fun () -> Ok {
                    Version = RepositoryConfigVersion
                    Type = RepositoryType.Backup
                    Aliases = []
                    Rules = []
                }
            }
            SaveSyncInstructionsFile = instructionsSaved.AddRange >> Ok
            OpenSyncInstructionsForUserEdition = fun () -> Ok ()
            AreInstructionsAccepted = fun () -> Ok true
            SubmitSyncInstructions = instructionsSubmitted.AddRange >> Ok
            SaveTargetBackupRules = rulesSaved.AddRange >> Ok
            UpdateTargetTrackFile = Ok
        }

        let result = replicateBackup infra
        test <@ result = Ok "Synchronization completed!" @>

        let expected = [
            Add d1.Path
            Add d2f2.Path
            Add d2f3.Path
        ]

        test <@ instructionsSaved |> Seq.toList = expected @>
        test <@ instructionsSubmitted |> Seq.toList = expected @>
        test <@ rulesSaved |> Seq.toList = sourceRules @>

    [<Fact>]
    let ``load data then not submit instructions and save rules when refused`` () =
        let instructionsSubmitted = System.Collections.Generic.List<_> ()
        let infra = {
            LoadSourceBackup = {
                LoadElements = fun () -> Ok [d1; d2; d2f1; d2f2; d2f3]
                LoadConfig = fun () -> Ok {
                    Version = RepositoryConfigVersion
                    Type = RepositoryType.Backup
                    Aliases = []
                    Rules = [
                        { Path = d2f2.Path; SyncRule = NotSave }
                        { Path = d2f3.Path; SyncRule = AlwaysReplace }
                        { Path = d2f4.Path; SyncRule = NotDelete }
                    ]
                }
            }
            LoadTargetBackup = {
                LoadElements = fun () -> Ok []
                LoadConfig = fun () -> Ok {
                    Version = RepositoryConfigVersion
                    Type = RepositoryType.Backup
                    Aliases = []
                    Rules = []
                }
            }
            SaveSyncInstructionsFile = ignore >> Ok
            OpenSyncInstructionsForUserEdition = fun () -> Ok ()
            AreInstructionsAccepted = fun () -> Ok false
            SubmitSyncInstructions = instructionsSubmitted.AddRange >> Ok
            SaveTargetBackupRules = fun _ -> Error "Should not be called"
            UpdateTargetTrackFile = fun () -> Error "Should not be called"
        }

        let result = replicateBackup infra
        test <@ result = Ok "Synchronization aborted!" @>
        test <@ instructionsSubmitted |> Seq.isEmpty @>
