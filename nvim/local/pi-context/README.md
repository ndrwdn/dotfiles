# Neovim / terminal Pi context

This directory is a local Neovim plugin, loaded by lazy.nvim from
`nvim/init.lua`. Its `lua/pi-context.lua` module adds workspace diagnostics and
declares `omaclaren/pi-nvim-context` as a dependency in the lazy.nvim spec.

Run Neovim and Pi in separate tmux panes. Both use `pi-nvim-context`; the old
`pi-nvim` bridge has been removed. No CodeCompanion or pi-acp process is needed.

Fully restart Pi and Neovim after package changes. Run both from the same
project directory, then use `Space pp` (`:PiContextPick`) to explicitly link Pi.
Selections are scoped to Neovim's working directory. Cross-directory links must
be explicitly selected in the picker; there is no silent other-project fallback.

## Draft-only actions

| Mapping | Adds to Pi's editable draft / action |
| --- | --- |
| `Space pp` | Pick/link Pi session |
| `Space pf` | File reference |
| `Space pl` | Cursor location and current line |
| Visual `Space ps` | Selection |
| Visual `Space pS` | Selection with comment |
| `Space pd` | Current-buffer diagnostics (up to 50) |
| `Space pb` | Complete in-memory buffer |
| `Space pi` | Bridge status |
| `Space ph` | Toggle annotation explanation in draft |
| `Space pD` | Workspace diagnostics snapshot |

Context accumulates in the draft without submitting. Switch to Pi, add your
question, then press Enter when ready. Direct suggestion commands remain
available upstream but have no mappings here; temporary Tab acceptance is off.

## Workspace diagnostics

`Space pD` / `:PiSendDiagnostics` gathers all cached diagnostics within the
current buffer's Git workspace, falling back to Neovim's cwd outside Git.
It includes rust-analyzer / cargo-check messages already published to Neovim,
not the entire raw compiler output stream.

The snapshot contains absolute paths, 1-based locations, severity, source,
error codes, full messages, and nearby source lines. Modified buffers are
labelled `UNSAVED` and use in-memory source; unloaded files are read from disk
without loading them. The command does not save source files, request LSP
updates, or run checks. Diagnostics may still be stale or incomplete.

The snapshot opens in an unnamed, unlisted, read-only buffer. This lets us use
pi-nvim-context's **public `add_buffer()` API**, sharing exactly the same link
and draft-only transport as the other context actions, without private APIs or
a separate socket client. It stays open for review or an asynchronous session
picker; use `:close` to return to your source window. Subsequent invocations
reuse the snapshot buffer/window. Snapshots above 180 KiB are rejected rather
than silently truncated (assuming the upstream default payload limits).

The draft includes instructions not to rerun checks merely to rediscover these
errors; verification after changes is still allowed. Nothing sends on save.
The legacy command name `PiSendDiagnostics` is retained, but **no longer submits**.

## Installation and troubleshooting

The Pi half is installed in personal settings with:

```sh
pi install npm:pi-nvim-context
```

The Neovim half is managed by lazy.nvim. Fully restart both after changes;
Pi's `/reload` is not sufficient. Use `/nvim-context` in Pi and
`:PiContextStatus` in Neovim to inspect the bridge.

The Markdown-plan opening/return workflow is not implemented. After saving
inline answers, use `Space pf` to add the plan reference and ask Pi to reread it.

## Tests

From the dotfiles root, after installing plugins:

```sh
nvim --headless -u NONE -l nvim/local/pi-context/tests/pi-context.lua
```

Tests use real Neovim diagnostics, the public pi-nvim-context picker and buffer
API, and a local Unix-socket fixture. They verify draft `prefill` messages rather
than submitted prompts, link reuse, workspace filtering, unsaved/unloaded source,
and empty diagnostics. They never contact an actual Pi conversation or model.
