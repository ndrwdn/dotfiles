# Pi configuration

Selected public configuration for `~/.pi/agent`, linked by
`~/dotfiles/makesymlinks.sh`. Pi's agent directory itself remains local.

## Linked resources

- `AGENTS.md`
- `settings.json`
- `keybindings.json`
- `pi-vimmode.config.js`
- `themes/` (whole directory)
- `extensions/subagent/config.json`
- `extensions/firecrawl.json` (uses `FIRECRAWL_API_KEY`, not a literal key)

The Pi block creates missing parent directories and links only missing paths.
Existing files, directories, and symlinks—including broken links—are skipped.
It never replaces existing Pi configuration or makes backups automatically.
The general script also manages other dotfiles; it has no dry-run mode.

## Local-only files

`models.json` and `mcp.json` stay local, are not included here, and are not managed
by the symlink script. Credentials, sessions, trust decisions, caches, and installed
packages also remain local. Back up valuable local files separately.

The default provider/model in `settings.json` requires your local Quasar
configuration. Supply credentials through your private environment/auth files.
Never put secrets in this public directory.

## Packages

Install Pi:

```sh
curl -fsSL https://pi.dev/install.sh | sh
```

Restore extensions using the package declarations in `settings.json`; do not
copy installed `npm/`, `git/` or `install/` directories.
Declarations are currently unpinned. The Vim customization requires a pi-vimmode
release supporting `allowProtected`; old local patch artifacts were not migrated.
