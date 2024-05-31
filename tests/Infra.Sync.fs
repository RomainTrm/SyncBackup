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

    module ``areInstructionsAccepted should`` =
        [<Fact>]
        let ``return error if file is missing`` () =
            let uniqueTestDirectory = "test-eb63cf53-3de0-4402-b4a3-5fd0c727f879"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let result = InstructionsFile.areInstructionsAccepted path
            test <@ Result.isError result @>

        [<Fact>]
        let ``return false if accept is commented`` () =
            let uniqueTestDirectory = "test-9e2d72b5-629d-4761-ab36-12ed1a241f83"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let _ = InstructionsFile.save path "whatever" []

            let result = InstructionsFile.areInstructionsAccepted path
            test <@ result = Ok false @>

        [<Theory>]
        [<InlineData("  Accept")>]
        [<InlineData("Accept")>]
        [<InlineData("    Accept  ")>]
        let ``return true if accept is uncommented`` accept =
            let uniqueTestDirectory = "test-cf485f28-f85f-453d-83b7-f46514edfccd"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let _ = InstructionsFile.save path "whatever" []
            let filePath = Dsl.getSyncInstructionsFilePath path
            let fileContent = System.IO.File.ReadAllText filePath
            let acceptedFileContent = fileContent.Replace("# Accept", accept)
            System.IO.File.WriteAllText (filePath, acceptedFileContent)

            let result = InstructionsFile.areInstructionsAccepted path
            test <@ result = Ok true @>
