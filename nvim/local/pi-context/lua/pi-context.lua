-- Context handoffs to an existing terminal Pi session; never launches checks.
local M = {}

local function canonical(path)
  return vim.fs.normalize(vim.uv.fs_realpath(path) or path)
end

local function in_workspace(path, root)
  return path == root or vim.startswith(path, root == '/' and '/' or root .. '/')
end

--- Snapshot all diagnostics Neovim already holds for this workspace.
--- Does not load/save buffers or request diagnostics from an LSP server.
function M.snapshot(root)
  root = canonical(root)
  local entries = {}
  for _, diagnostic in ipairs(vim.diagnostic.get()) do
    local name = vim.api.nvim_buf_get_name(diagnostic.bufnr)
    if name ~= '' and in_workspace(canonical(name), root) then
      entries[#entries + 1] = { diagnostic = diagnostic, path = name }
    end
  end
  if #entries == 0 then return nil, 0 end
  table.sort(entries, function(a, b)
    if a.path ~= b.path then return a.path < b.path end
    if a.diagnostic.lnum ~= b.diagnostic.lnum then return a.diagnostic.lnum < b.diagnostic.lnum end
    return a.diagnostic.col < b.diagnostic.col
  end)

  local lines = {
    '# Neovim workspace diagnostics snapshot',
    'Workspace: ' .. root,
    'Captured: ' .. os.date('!%Y-%m-%dT%H:%M:%SZ'),
    'These are cached diagnostics already reported by Neovim/LSP (including rust-analyzer cargo checks).',
    'Do not rerun checks merely to rediscover these errors. Use this snapshot as the starting point.',
    'It may be stale or incomplete, especially while checks are running. Verify after changes when needed.',
    'UNSAVED context is from in-memory buffers and may not match disk or the last check.',
    '',
  }
  for _, entry in ipairs(entries) do
    local d = entry.diagnostic
    local code = d.code or (d.user_data and d.user_data.lsp and d.user_data.lsp.code)
    local source = d.source or 'unknown source'
    if code ~= nil then source = source .. ' / ' .. tostring(code) end
    local unsaved = vim.bo[d.bufnr].modified and ' [UNSAVED]' or ''
    lines[#lines + 1] = string.format('## %s:%d:%d%s', entry.path, d.lnum + 1, d.col + 1, unsaved)
    lines[#lines + 1] = string.format('%s [%s]', vim.diagnostic.severity[d.severity or vim.diagnostic.severity.ERROR], source)
    lines[#lines + 1] = d.message
    local first = math.max(0, d.lnum - 2)
    local last = d.lnum + 3
    local context
    if vim.api.nvim_buf_is_loaded(d.bufnr) then
      context = vim.api.nvim_buf_get_lines(d.bufnr, first, last, false)
    else
      -- Reading disk avoids BufRead/FileType events and consequent LSP checks.
      local ok, disk = pcall(vim.fn.readfile, entry.path, '', last)
      if ok then context = vim.list_slice(disk, first + 1, last) end
    end
    if context and #context > 0 then
      lines[#lines + 1] = 'Source context (line-numbered):'
      for i, line in ipairs(context) do
        lines[#lines + 1] = string.format('    %d | %s', first + i, line)
      end
    else
      lines[#lines + 1] = '(Source context unavailable.)'
    end
    lines[#lines + 1] = ''
  end
  return table.concat(lines, '\n'), #entries
end

-- Use the upstream public buffer-sharing API so linking, payload validation,
-- and draft-only delivery stay owned by pi-nvim-context, not a second bridge.
local snapshot_buffer
function M.send_diagnostics(root)
  root = root or vim.fs.root(0, { '.git' }) or vim.fn.getcwd()
  local message = M.snapshot(root)
  if not message then
    vim.notify('No cached diagnostics in this workspace to add to Pi', vim.log.levels.INFO)
    return
  end
  -- Leave room for upstream's buffer wrapper and avoid silent truncation.
  if #message > 180 * 1024 then
    vim.notify('Workspace diagnostics exceed the draft handoff limit (180 KiB); nothing sent.', vim.log.levels.WARN)
    return
  end
  if not snapshot_buffer or not vim.api.nvim_buf_is_valid(snapshot_buffer) then
    -- Upstream add_buffer requires a normal buffer (not buftype=nofile).
    -- Keep this unnamed/unlisted and read-only; it has no backing file.
    snapshot_buffer = vim.api.nvim_create_buf(false, false)
    vim.bo[snapshot_buffer].bufhidden = 'hide'
    vim.bo[snapshot_buffer].swapfile = false
  end
  vim.bo[snapshot_buffer].readonly = false
  vim.bo[snapshot_buffer].modifiable = true
  vim.api.nvim_buf_set_lines(snapshot_buffer, 0, -1, false, vim.split(message, '\n', { plain = true }))
  vim.bo[snapshot_buffer].modified = false
  vim.bo[snapshot_buffer].readonly = true
  vim.bo[snapshot_buffer].modifiable = false
  local window = vim.fn.bufwinid(snapshot_buffer)
  if window ~= -1 then
    vim.api.nvim_set_current_win(window)
  else
    vim.cmd('botright split')
    vim.api.nvim_win_set_buf(0, snapshot_buffer)
  end
  -- Keep the snapshot alive if the public API needs an asynchronous picker.
  -- It is also visible for review; :close returns to the original source.
  require('pi-nvim-context').add_buffer()
end

function M.setup()
  vim.api.nvim_create_user_command('PiSendDiagnostics', function() M.send_diagnostics() end, {
    desc = 'Add cached workspace diagnostics to Pi draft (no checks or submission)',
  })
  vim.keymap.set('n', '<leader>pD', '<Cmd>PiSendDiagnostics<CR>', { desc = 'Send workspace LSP diagnostics to Pi' })
end

return M
