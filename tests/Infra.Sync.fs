module SyncBackup.Tests.Infra.Sync

open SyncBackup.Domain.Dsl
open Xunit
open Swensen.Unquote
open SyncBackup.Tests.Infra

open SyncBackup.Domain.Sync
open SyncBackup.Infra
open SyncBackup.Infra.Sync

module InstructionsFile =
    module ``save should`` =
        [<Fact>]
        let ``no instructions should display "up to date"`` () =
            let uniqueTestDirectory = "test-57e4353c-f89a-4081-85e7-56077fdc147a"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let result = InstructionsFile.save path "whatever" []
            test <@ result = Ok () @>

            let fileContent = Dsl.getSyncInstructionsFilePath path |> System.IO.File.ReadAllLines
            test <@ fileContent |> Seq.contains "# No change, your backup is up to date."  @>

        [<Fact>]
        let ``persist instructions`` () =
            let uniqueTestDirectory = "test-1871011a-9f98-4058-aa6f-171de28c383d"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let result = InstructionsFile.save path "whatever" [
                SyncInstruction.Add { Value = "path1"; Type = Alias; ContentType = ContentType.File }
                SyncInstruction.Replace { Value = "path2"; Type = Source; ContentType = ContentType.Directory }
                SyncInstruction.Delete { Value = "path3"; Type = Source; ContentType = ContentType.File }
            ]
            test <@ result = Ok () @>

            let fileContent = Dsl.getSyncInstructionsFilePath path |> System.IO.File.ReadAllLines
            let expected = [|
                "- Add: file::\"*path1\""
                "- Replace: dir::\"path2\""
                "- Delete: file::\"path3\""
            |]
            test <@ (Set fileContent) |> Set.isSubset (Set expected)  @>
