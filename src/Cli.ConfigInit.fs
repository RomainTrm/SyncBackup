module SyncBackup.Cli.ConfigInit

open Argu

type Init = | [<Hidden>] NoOption
with interface IArgParserTemplate with member this.Usage = ""

let runCommand commandInfra =
    SyncBackup.Commands.Config.Init.run commandInfra
    |> Result.map (fun () -> "Repository initialized")
