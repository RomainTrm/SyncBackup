# SyncBackup

## Introduction

Homemade tools to synchronize my backups with some custom rules.  
It's a CLI tool influenced by my daily Git CLI usage.  
It provides a set of rules to handle delta between repositories, you may want to save different subsets of your data depending on your backup support (Cloud, hard-drive).

## Technical requirements

- [VS Code](https://code.visualstudio.com/Download) is installed on your computer and open if you type ``code`` on a terminal

## Installation

### Option 1: Release

- Download it
- Unzip the archive wherever you want
- Add the directory containing the `Sync.exe` to the PATH in your Environment Variables

### Option 2: Compile it

- Make sure [.Net 8.0 SDK](https://dotnet.microsoft.com/en-us/download/dotnet/8.0) or greater is installed on your computer
- Clone the repository then build the project
- Add the output directory to the PATH in your Environment Variables

### Test your installation

- Run a new terminal and type ``sync``, it should display:

```text
USAGE: Sync [--help] [<subcommand> [<options>]]

SUBCOMMANDS:

    init <options>        Initialize the current directory as a repository to synchronize.
    alias <options>       Manage aliases (pointers to directories outside the repository's directory), only available for the source repository.
    rules <options>       Manage rules for synchronization.
    scan <options>        Reference all directories and files in the repository.
    process <options>     Run synchronization process between two repositories.

    Use 'Sync <subcommand> --help' for additional information.

OPTIONS:

    --help                display this list of options.
```

## How to use

### Setup Source repository

- Initialize repository: ``sync init --source``
- (optional) Add aliases to add content placed outside the current directory: ``sync alias --add [name] [path]``
- Scan repository content: ``sync scan --run``, it will display changes since the last scan, you can specify rules 
- (optional) Add some rules: ``sync rules --add [Path] [Rule]`` or edit existing ones: ``sync rules --edit``

### Setup Backup repository

- Initialize repository ``sync init --backup``
- Scan repository content: ``sync scan --run``, it will display changes since the last scan, you can specify rules
- (optional) Add some rules: ``sync rules --add [Path] [Rule]`` or edit existing ones: ``sync rules --edit``

### Run synchronization

From the source repository:

- Run: ``sync process --backuppath [Path]``
- Check the actions to apply, accept it by uncommenting the line ``accept``

```text
# Synchronizing from:
#     [Source path]
# to:
#     [Backup path]
# If you accept the following changes, uncomment the next line (remove #) and save:
# Accept

[Instructions...]
```

- Close the file
- Wait until synchronization is completed

## Limitations

`sync` doesn't track file changes (renaming, moving, updating)

- renaming and moving are handled if you don't specify rules, existing backup will be deleted and new version saved
- updating can be handled if you specify a `replace` rule (note: it replaces items at every synchronization)

## Upcoming features

- editing rules through the scan file
- duplicating a backup repository (not a restore backup feature)
