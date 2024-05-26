module SyncBackup.Tests.Properties.CustomGenerators

open System
open FsCheck

type NonWhiteSpaceStringGenerator () =
    static member String() = {
        new Arbitrary<String>() with
            override x.Generator =
                Arb.Default.NonWhiteSpaceString ()
                |> Arb.toGen
                |> Gen.map _.Get
        }

type PathStringGenerator () =
    static member String() = {
        new Arbitrary<String>() with
            override x.Generator =
                Arb.Default.UnicodeString ()
                |> Arb.toGen
                |> Gen.map _.Get
        }
