module SyncBackup.Tests.Commands.Sync

open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Domain.Sync
open SyncBackup.Commands.Sync

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
    }

    [<Fact>]
    let ``return error if designated source repository is a backup repository`` () =
        let result = sync {
            defaultInfra with
                LoadSource = {
                    LoadElements = fun () -> Ok []
                    LoadConfig = fun () -> Ok {
                        Type = RepositoryType.Backup
                        Aliases = []
                        Rules = []
                    }
                }
                LoadBackup = {
                    LoadElements = fun () -> Ok []
                    LoadConfig = fun () -> Ok {
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
                        Type = RepositoryType.Source
                        Aliases = []
                        Rules = []
                    }
                }
                LoadBackup = {
                    LoadElements = fun () -> Ok []
                    LoadConfig = fun () -> Ok {
                        Type = RepositoryType.Source
                        Aliases = []
                        Rules = []
                    }
                }
        }

        test <@ Result.isError result @>

    let d1 = { Type = Source; Value = "d1"; ContentType = Directory }
    let d2 = { Type = Source; Value = "d2"; ContentType = Directory }
    let d2f1 = { Type = Source; Value = "d2/f1"; ContentType = File }
    let d2f2 = { Type = Source; Value = "d2/f2"; ContentType = File }
    let d2f3 = { Type = Source; Value = "d2/f3"; ContentType = File }
    let d2f4 = { Type = Source; Value = "d2/f4"; ContentType = File }

    [<Fact>]
    let ``load data then submit instructions when accepted`` () =
        let instructionsSaved = System.Collections.Generic.List<_> ()
        let instructionsSubmitted = System.Collections.Generic.List<_> ()
        let infra = {
            LoadSource = {
                LoadElements = fun () -> Ok [d1; d2; d2f1; d2f2; d2f3]
                LoadConfig = fun () -> Ok {
                    Type = RepositoryType.Source
                    Aliases = []
                    Rules = [{ Path = d1; SyncRule = Exclude }]
                }
            }
            LoadBackup = {
                LoadElements = fun () -> Ok [d2; d2f3; d2f4]
                LoadConfig = fun () -> Ok {
                    Type = RepositoryType.Backup
                    Aliases = []
                    Rules = [
                        { Path = d2f2; SyncRule = NotSave }
                        { Path = d2f3; SyncRule = AlwaysReplace }
                        { Path = d2f4; SyncRule = NotDelete }
                    ]
                }
            }
            SaveSyncInstructionsFile = instructionsSaved.AddRange >> Ok
            OpenSyncInstructionsForUserEdition = fun () -> Ok ()
            AreInstructionsAccepted = fun () -> Ok true
            SubmitSyncInstructions = instructionsSubmitted.AddRange >> Ok
        }

        let result = sync infra
        test <@ result = Ok "Synchronization completed!" @>

        let expected = [
            Add d2f1
            Replace d2f3
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
                    Type = RepositoryType.Source
                    Aliases = []
                    Rules = [{ Path = d1; SyncRule = Exclude }]
                }
            }
            LoadBackup = {
                LoadElements = fun () -> Ok [d2; d2f3; d2f4]
                LoadConfig = fun () -> Ok {
                    Type = RepositoryType.Backup
                    Aliases = []
                    Rules = [
                        { Path = d2f2; SyncRule = NotSave }
                        { Path = d2f3; SyncRule = AlwaysReplace }
                        { Path = d2f4; SyncRule = NotDelete }
                    ]
                }
            }
            SaveSyncInstructionsFile = ignore >> Ok
            OpenSyncInstructionsForUserEdition = fun () -> Ok ()
            AreInstructionsAccepted = fun () -> Ok false
            SubmitSyncInstructions = instructionsSubmitted.AddRange >> Ok
        }

        let result = sync infra
        test <@ result = Ok "Synchronization aborted!" @>
        test <@ instructionsSubmitted |> Seq.isEmpty @>
