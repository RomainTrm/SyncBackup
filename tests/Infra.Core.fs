module SyncBackup.Tests.Infra.Core

open System
open Xunit
open Swensen.Unquote
open SyncBackup.Infra.Core

module ``checkPathExists should`` =
    [<Fact>]
    let ``return ok if exists`` () =
        let path = Environment.CurrentDirectory
        let result = checkPathExists path
        test <@ result = Ok () @>

    [<Fact>]
    let ``return error if doesn't exist`` () =
        let path = "wrong path"
        let result = checkPathExists path
        test <@ result = Error "The specified directory path doesn't exist" @>
