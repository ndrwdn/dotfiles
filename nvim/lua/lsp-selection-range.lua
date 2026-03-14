-- taken from
-- https://github.com/ptimoney/lsp-selection-range.nvim/blob/028624b3b721f4b5b8a3724a99aa350b491ed01b/lua/lsp-selection-range.lua
-- changing up to allow custom keybindings

local M = {}

local default_config = {
  clear_state_on_mode_exit = true,
}

local config = vim.deepcopy(default_config)
local selection_state = {}

local function apply_range(bufnr, client, range)
  local start_line = range.start.line + 1
  local end_line = range["end"].line + 1
  local start_text = vim.api.nvim_buf_get_lines(bufnr, range.start.line, range.start.line + 1, false)[1] or ""
  local end_text = vim.api.nvim_buf_get_lines(bufnr, range["end"].line, range["end"].line + 1, false)[1] or ""

  local start_col, end_col
  if vim.str_byteindex then
    start_col = vim.str_byteindex(start_text, range.start.character, true) + 1
    end_col = vim.str_byteindex(end_text, range["end"].character, true) + 1
  else
    start_col = range.start.character + 1
    end_col = range["end"].character + 1
  end

  vim.fn.setpos("'<", { bufnr, start_line, start_col, 0 })
  vim.fn.setpos("'>", { bufnr, end_line, end_col, 0 })
  vim.cmd("normal! gv")
end

local function selection_range(shrink)
  local bufnr = vim.api.nvim_get_current_buf()
  local client = vim.lsp.get_clients({ bufnr = bufnr })[1]
  if not client then
    return
  end

  local state = selection_state[bufnr]
  if state and state.chain then
    if shrink and state.index > 1 then
      state.index = state.index - 1
      apply_range(bufnr, client, state.chain[state.index])
    elseif not shrink and state.index < #state.chain then
      state.index = state.index + 1
      apply_range(bufnr, client, state.chain[state.index])
    end
    return
  end

  local pos_params = vim.lsp.util.make_position_params(0, client.offset_encoding)
  vim.lsp.buf_request(bufnr, "textDocument/selectionRange", {
    textDocument = pos_params.textDocument,
    positions = { pos_params.position },
  }, function(err, result)
    if err or not result or #result == 0 then
      return
    end

    local chain = {}
    local current = result[1]
    while current do
      table.insert(chain, current.range)
      current = current.parent
    end

    selection_state[bufnr] = { chain = chain, index = 1 }
    apply_range(bufnr, client, chain[1])
  end)
end

function M.setup(user_config)
  config = vim.tbl_deep_extend("force", default_config, user_config or {})

  if config.clear_state_on_mode_exit then
    vim.api.nvim_create_autocmd("ModeChanged", {
      group = vim.api.nvim_create_augroup("lsp_selection_range_clear", { clear = true }),
      pattern = { "v:*", "V:*", "\x16:*" },
      callback = function()
        selection_state[vim.api.nvim_get_current_buf()] = nil
      end,
    })
  end
end

function M.selection_range(shrink)
  selection_range(shrink)
end

return M
