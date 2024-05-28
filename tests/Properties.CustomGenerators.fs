module SyncBackup.Tests.Properties.CustomGenerators

open System
open FsCheck
open SyncBackup.Domain

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
                |> Gen.filter (not<<String.IsNullOrWhiteSpace)
        }

type SourceRepositoryRulesOnlyGenerator () =
    static member String() = {
        new Arbitrary<Dsl.SyncRules>() with
            override x.Generator =
                Dsl.RepositoryType.Source
                |> Dsl.SyncRules.getRulesAvailable
                |> Gen.elements
        }
