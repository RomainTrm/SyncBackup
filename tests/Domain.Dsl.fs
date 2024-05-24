module SyncBackup.Tests.Domain.Dsl

open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl

module ``build directory path should`` =

    [<Theory>]
    [<InlineData("C:\\", "C:")>]
    [<InlineData("C:\\someDir\\", "C:\\someDir")>]
    [<InlineData("C:\\someDir", "C:\\someDir")>]
    [<InlineData("C:\\someDir\"", "C:\\someDir")>]
    let ``cleanup directory path`` input expected =
        let result = DirectoryPath.build input
        test <@ result = expected @>
