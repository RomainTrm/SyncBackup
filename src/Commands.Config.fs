module SyncBackup.Commands.Config

open SyncBackup.Domain.Dsl

type Infra = {
    InitConfig: RepositoryPath -> RepositoryConfig -> Result<unit, string>
}

module Init =
    let run (infra: Infra) (repositoryPath: RepositoryPath) =
        let config = {
            IsMainRepository = true
            Aliases = []
        }
        infra.InitConfig repositoryPath config
