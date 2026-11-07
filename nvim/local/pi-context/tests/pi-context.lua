-- Run: nvim --headless -u NONE -l nvim/local/pi-context/tests/pi-context.lua
local plugin_root = vim.fs.dirname(vim.fs.dirname(debug.getinfo(1, 'S').source:sub(2)))
vim.opt.rtp:append(vim.fn.fnamemodify(plugin_root, ':p'))
vim.opt.rtp:append(vim.fn.stdpath('data') .. '/lazy/pi-nvim-context')
local runtime = vim.fn.tempname()
vim.fn.mkdir(runtime, 'p', 448)
vim.env.PI_NVIM_CONTEXT_RUNTIME_DIR = runtime
local root = vim.fn.tempname()
vim.fn.mkdir(root .. '/src', 'p')
vim.fn.mkdir(root .. '-other', 'p')
local servers = {}
local function contains(text, part)
  assert(text:find(part, 1, true), 'Missing in snapshot: ' .. part)
end
local function buffer(path, lines)
  local b = vim.fn.bufadd(path)
  vim.fn.bufload(b)
  vim.api.nvim_buf_set_lines(b, 0, -1, false, lines)
  return b
end
local function server(cwd)
  local path = runtime .. '/test.sock'
  local info = { protocol = 1, cwd = cwd, pid = vim.fn.getpid(), socketPath = path,
    sessionId = 'test-session', capabilities = { 'prefill' } }
  local handle = assert(vim.uv.new_pipe(false))
  handle:bind(path)
  local received = {}
  handle:listen(8, function(err)
    assert(not err, err)
    local client = assert(vim.uv.new_pipe(false))
    handle:accept(client)
    local pending = ''
    client:read_start(function(_, data)
      if not data then return end
      pending = pending .. data
      if not pending:find('\n', 1, true) then return end
      local request = vim.json.decode(pending)
      if request.type ~= 'ping' then received[#received + 1] = request end
      client:write(vim.json.encode({ ok = true, type = request.type == 'ping' and 'ping' or 'prefilled', info = info }) .. '\n', function() client:close() end)
    end)
  end)
  vim.fn.writefile({ vim.json.encode(info) }, runtime .. '/test.json')
  servers[#servers + 1] = handle
  return received
end
local ok, err = xpcall(function()
  local context = require('pi-context')
  local b = buffer(root .. '/src/main.rs', { 'fn main() {', '    missing();', '}' })
  local other = buffer(root .. '-other/secret.rs', { 'private context' })
  local ns = vim.api.nvim_create_namespace('pi-context-tests')
  vim.diagnostic.set(ns, b, {
    { lnum = 1, col = 4, end_lnum = 1, end_col = 11, severity = vim.diagnostic.severity.ERROR,
      source = 'rust-analyzer', code = 'E0425', message = 'cannot find function `missing`\nhelp: define it' },
  })
  vim.diagnostic.set(ns, other, { { lnum = 0, col = 0, message = 'outside workspace' } })
  local text, count = context.snapshot(root)
  assert(count == 1, 'Diagnostics must be scoped to workspace, not similarly-prefixed paths')
  contains(text, root .. '/src/main.rs:2:5')
  contains(text, 'ERROR')
  contains(text, 'rust-analyzer')
  contains(text, 'E0425')
  contains(text, 'help: define it')
  contains(text, 'missing();')
  contains(text, 'UNSAVED')
  contains(text, 'Do not rerun checks merely to rediscover')
  assert(not text:find('outside workspace', 1, true))
  assert(vim.bo[b].modified, 'Sending context must not save buffers')

  -- Unloaded buffers must use disk context without loading them / triggering LSP.
  local disk_path = root .. '/src/lib.rs'
  vim.fn.writefile({ 'pub fn helper() {}' }, disk_path)
  local disk = vim.fn.bufadd(disk_path)
  vim.diagnostic.set(ns, disk, { { lnum = 0, col = 0, severity = 2, source = 'cargo check', message = 'unused helper' } })
  text, count = context.snapshot(root)
  assert(count == 2)
  contains(text, 'pub fn helper() {}')
  assert(not vim.api.nvim_buf_is_loaded(disk), 'Snapshot must not load diagnostic buffers')

  vim.cmd.cd(root)
  local bridge = require('pi-nvim-context')
  bridge.setup({ keymaps = false, notify = false })
  local received = server(root)
  local picks = 0
  vim.ui.select = function(items, _, callback) picks = picks + 1; callback(items[1]) end
  -- Exercise public linking and real socket transport; no private test APIs.
  bridge.pick()
  assert(vim.wait(2000, function() return picks == 1 end), 'Session picker did not open')
  context.send_diagnostics(root)
  assert(vim.wait(2000, function() return #received == 1 end), 'No draft context delivered')
  assert(received[1].type == 'prefill', 'Workspace diagnostics must never submit a prompt')
  contains(received[1].text, 'E0425')
  contains(received[1].text, 'unused helper')
  assert(picks == 1, 'Must reuse the public PiContextPick session link')
  assert(vim.bo[b].modified, 'Source must remain unsaved')
  assert(vim.bo.readonly and not vim.bo.modifiable, 'Snapshot should be read-only')
  context.send_diagnostics(root)
  assert(vim.wait(2000, function() return #received == 2 end))
  assert(picks == 1)

  vim.diagnostic.set(ns, b, { { lnum = 1, col = 0, message = string.rep('x', 181 * 1024) } })
  context.send_diagnostics(root)
  vim.wait(50, function() return false end)
  assert(#received == 2, 'Oversized snapshots must be rejected, not truncated')
  vim.diagnostic.reset(ns)
  local empty, n = context.snapshot(root)
  assert(empty == nil and n == 0, 'No diagnostics should not produce an agent turn')
  local deliveries = #received
  context.send_diagnostics(root)
  vim.wait(50, function() return false end)
  assert(#received == deliveries, 'Empty diagnostics must not be sent')
  print('PASS: workspace snapshots, unsaved/unloaded source, shared session link, draft-only delivery, size limit, empty diagnostics')
end, debug.traceback)
for _, handle in ipairs(servers) do handle:close() end
vim.fn.delete(runtime, 'rf')
vim.cmd.cd('/')
vim.fn.delete(root, 'rf')
vim.fn.delete(root .. '-other', 'rf')
if not ok then io.stderr:write(err .. '\n'); vim.cmd('cquit 1') end
vim.cmd('qa!')
