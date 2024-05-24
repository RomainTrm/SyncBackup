module SyncBackup.Infra.Editor

module VsCode =
    open SyncBackup.Domain.Dsl
    open System.Diagnostics

    let runEditor (fileToEdit: FilePath) =
        try
            let processStartInfo = ProcessStartInfo ("code", ["--wait"; fileToEdit])
            processStartInfo.UseShellExecute <- true
            processStartInfo.WindowStyle <- ProcessWindowStyle.Hidden
            let vsCode = Process.Start processStartInfo
            vsCode.WaitForExitAsync ()
            |> Async.AwaitTask
            |> Async.RunSynchronously
            Ok ()

        with e ->
            Error (e.ToString ())
