module [<AutoOpen>] SyncBackup.Helpers

type ResultBuilder() =
    member x.Bind(comp, func) = Result.bind func comp
    member x.Return(value) = Ok value
    member x.ReturnFrom(value) = value

let result = ResultBuilder ()
