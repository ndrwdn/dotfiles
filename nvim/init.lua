-- General settings
-- ----------------
-- Don't use swapfiles
vim.o.swapfile = false

-- Searching
vim.o.ignorecase = true
vim.o.smartcase = true

-- Display settings
-- 'c' shows '$' at end of text.
vim.opt.cpoptions:append("$")

-- Vertical splits for diffing
vim.opt.diffopt:append("vertical")

-- Show line numbers
vim.o.number = true

-- Open new windows to the bottom/right instead of top/left
vim.o.splitbelow = true
vim.o.splitright = true

-- Don't show intro message
vim.opt.shortmess:append("I")

-- Statusline and window title
vim.o.statusline = [[%t %3(%m%)%4(%r%)%y [%{&ff}]%=Line: %-10(%l/%L%) Col: %-4(%c%) Buf: %-2(%n%) %11([%3b][0x%02B]%)]]
vim.o.titlestring = [[%t %m%( (%{expand("%:p:~:h")})%)]]

-- Command-line auto-completion
vim.o.wildmode = 'full:lastused'

-- Mouse settings
vim.o.mousehide = false
vim.o.mouse = 'a'

-- Faster display
vim.o.timeoutlen = 500

-- Indentation settings
vim.o.expandtab = true
vim.o.shiftround = true
vim.o.smartindent = true
vim.o.shiftwidth = 2
vim.o.softtabstop = 2
vim.o.tabstop = 2

-- Formatting for set list
vim.o.listchars = 'tab:>.,trail:.,extends:#,nbsp:.'

-- netrw config
vim.g.netrw_banner = 0
vim.g.netrw_keepdir = 0
vim.g.netrw_liststyle = 0
vim.g.netrw_browse_split = 0
vim.g.netrw_preview = 1

-- Keybindings
-- -----------
-- Space is our leader, must be set ahead of keybindings using it
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Make Shift-Tab unindent in insert mode
vim.keymap.set('i', '<S-Tab>', '<C-d>')

-- Shortcut to toggle wrap
vim.keymap.set('n', '<localleader>w', ':set wrap!<cr>', { silent = true })

-- Shortcut to toggle paste
vim.keymap.set('n', '<localleader>p', ':set paste!<cr>', { silent = true })

-- Fix indentation for yaml files when commenting out a section
vim.api.nvim_create_autocmd("FileType", {
  pattern = "yaml",
  command = "setlocal expandtab indentkeys-=0#"
})

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- Setup lazy.nvim
require("lazy").setup({
  spec = {
    {
      'f-person/auto-dark-mode.nvim',
      lazy = false,
      priority = 1010,
      opts = {
        update_interval = 1000,
        set_dark_mode = function()
          vim.api.nvim_set_option_value("background", "dark", {})
        end,
        set_light_mode = function()
          vim.api.nvim_set_option_value("background", "light", {})
        end,
      },
    },
    {
      'maxmx03/solarized.nvim',
      lazy = false,
      priority = 1000,
      config = function()
        vim.o.background = 'light'
        vim.cmd.colorscheme 'solarized'
      end,
    },
    { 'tpope/vim-repeat' },
    { 'tpope/vim-surround' },
    { 'tpope/vim-vinegar' },
    { 'tpope/vim-unimpaired' },
  },
  install = { colorscheme = { "solarized" } },
  checker = { enabled = true },
})
