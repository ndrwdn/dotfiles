# Pi configuration

Selected public configuration for `~/.pi/agent`, linked by
`~/dotfiles/makesymlinks.sh`. Pi's agent directory itself remains local.

## Linked resources

- `AGENTS.md`
- `settings.json`
- `keybindings.json`
- `pi-vimmode.config.js`
- `themes/` (whole directory)
- `extensions/primary-model.js` (portable `primary/default` virtual model)
- `extensions/subagent/config.json`
- `extensions/firecrawl.json` (uses `FIRECRAWL_API_KEY`, not a literal key)

The Pi block creates missing parent directories and links only missing paths.
Existing files, directories, and symlinks—including broken links—are skipped.
It never replaces existing Pi configuration or makes backups automatically.
The general script also manages other dotfiles; it has no dry-run mode.

## Local-only files

`primary-model.json`, `models.json`, and `mcp.json` stay local, are not included here, and are not managed
by the symlink script. Credentials, sessions, trust decisions, caches, and installed
packages also remain local. Back up valuable local files separately.

## Workstation default model

Requires Pi 1.5.0 or later (virtual-model support). Shared settings select
`primary/default`; the extension routes it to a physical model configured locally.
Create `~/.pi/agent/primary-model.json` on each workstation, for example:

```json
{
  "provider": "qd",
  "model": "openai:gpt-6.1-sol"
}
```

Use the real provider and API model ID from that workstation’s catalog (`/model`),
not a display name or another virtual model. Keep custom provider definitions in
local `models.json`; built-in providers work too. No provider renaming is needed.
Supply credentials through private environment/auth files; never put secrets in
this public directory. The mapping itself needs no credentials.

The extension honors `PI_CODING_AGENT_DIR` (including `~/`); put the mapping in
that directory when using the override. Mapping changes apply on the next new
user/direct request. Tool continuations and retries stay on their dispatched
model. Selected thinking levels pass through; Pi clamps unsupported levels.
Missing/invalid mappings and unknown targets fail with setup guidance, rather
than choosing an arbitrary model.

After linking the extension, run `/reload` and select `primary/default` for an
existing session, or start a new session to use the shared default. Resumed
sessions retain their recorded model selection.

Run the extension tests with `node --test pi/tests/*.test.mjs` from the dotfiles
root (Node 24+), or `node --test tests/*.test.mjs` from `pi/`.

## Packages

Install Pi:

```sh
curl -fsSL https://pi.dev/install.sh | sh
```

Restore extensions using the package declarations in `settings.json`; do not
copy installed `npm/`, `git/` or `install/` directories.
Declarations are currently unpinned. The Vim customization requires a pi-vimmode
release supporting `allowProtected`; old local patch artifacts were not migrated.
