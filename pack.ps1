dotnet clean .\src\SyncBackup.fsproj -c Release
dotnet restore
paket restore
dotnet build .\src\SyncBackup.fsproj -c Release --sc

mkdir versions -Force
$syncVersion = .\src\bin\Release\net8.0\win-x64\sync version
$archivePath = ".\versions\sync-$syncVersion-win-x64.zip"

if (Test-Path $ArchivePath) {
    Write-Warning "sync-$syncVersion.zip already exists, increment version number."
}
else {
    Compress-Archive -Path .\src\bin\Release\net8.0\win-x64\* -DestinationPath $archivePath
    echo "Zip available in the versions directory."
}

git tag $syncVersion
git push origin $syncVersion
