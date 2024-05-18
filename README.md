# SyncBackup

Home made tool to synchronize my backups with some custom rules

## Concept

Note: following description is a draft, it has not been tested and will probably evolve during implementation

- Users must generate a "sync file" per duplication source (your computer and every backup emplacement)
- A "sync file" is tracing directories, subdirectories and files to synchronize
- Users can add rules to files and/or directories, examples: 
  - do not export to target
  - do not delete when missing from source
  - refuse from source
  - aliases for directories
  - ...
- On syncing, user designate source and target "sync file", then the tool displays changes to the target, user can edit rules before accepting
