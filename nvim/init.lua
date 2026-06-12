-- General settings
-- ----------------
-- Don't use swapfiles
vim.o.swapfile = false

-- Don't save marks and jumplist to shared data
vim.o.shada = "!,'0,<50,s10,h"

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
vim.o.statusline = [[%{expand('%:~:.')!=#''?expand('%:~:.'):'[No Name]'} %3(%m%)%4(%r%)%y [%{&ff}]%=Line: %-10(%l/%L%) Col: %-4(%c%) %11([%3b][0x%02B]%)]]
vim.o.titlestring = [[%{expand('%:~:.')!=#''?expand('%:~:.'):'[No Name]'} %m%( (%{expand("%:p:~:h")})%)]]

-- Command-line auto-completion
vim.o.wildmode = 'full:lastused'

-- Mouse settings
vim.o.mousehide = false
vim.o.mouse = 'a'

-- Faster display
vim.o.timeoutlen = 500

-- Keep some lines visible when scrolling
vim.o.scrolloff = 5

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

-- Quickfix helpers
vim.api.nvim_create_user_command('QFClear', 'cexpr []', {bang = true})
vim.api.nvim_create_user_command(
  'QFSearch',
  function(opts)
    vim.cmd("g/" .. opts.fargs[1] .. "/caddexpr expand(\"%\") . \":\" . line(\".\") . \":\" . getline(\".\")")
    vim.cmd("copen")
  end,
  {
    bang = true,
    nargs = 1
  })

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

-- Shortuct to clear current search
vim.keymap.set('n', '', ':nohlsearch<cr>', { silent = true })

-- Make it easier to yank to system clipboard
vim.keymap.set({'n', 'x'}, '<C-y>', '"+y', { silent = true })

-- Make it easy to copy filename to clipboard
vim.api.nvim_create_user_command('CopyFileName', 'let @*=expand("%:.")', {bang = true})
vim.keymap.set({'n', 'x'}, "<leader>c", "<Cmd>CopyFileName<CR>", { desc = "copy filename to system clipboard" })

-- Fix indentation for yaml files when commenting out a section
vim.api.nvim_create_autocmd("FileType", {
  pattern = "yaml",
  command = "setlocal expandtab indentkeys-=0#"
})

function get_visual_selection()
  local modeInfo = vim.api.nvim_get_mode()
  local mode = modeInfo.mode

  local cursor = vim.api.nvim_win_get_cursor(0)
  local cline, ccol = cursor[1], cursor[2]
  local vline, vcol = vim.fn.line('v'), vim.fn.col('v')

  local sline, scol
  local eline, ecol
  if cline == vline then
    if ccol <= vcol then
      sline, scol = cline, ccol
      eline, ecol = vline, vcol
      scol = scol + 1
    else
      sline, scol = vline, vcol
      eline, ecol = cline, ccol
      ecol = ecol + 1
    end
  elseif cline < vline then
    sline, scol = cline, ccol
    eline, ecol = vline, vcol
    scol = scol + 1
  else
    sline, scol = vline, vcol
    eline, ecol = cline, ccol
    ecol = ecol + 1
  end

  if mode == "V" or mode == "CTRL-V" or mode == "\22" then
    scol = 1
    ecol = nil
  end

  local lines = vim.api.nvim_buf_get_lines(0, sline - 1, eline, 0)
  if #lines == 0 then return end

  local startText, endText
  if #lines == 1 then
    startText = string.sub(lines[1], scol, ecol)
  else
    startText = string.sub(lines[1], scol)
    endText = string.sub(lines[#lines], 1, ecol)
  end

  local selection = {startText}
  if #lines > 2 then
    vim.list_extend(selection, vim.list_slice(lines, 2, #lines - 1))
  end
  table.insert(selection, endText)

  return selection
end

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

-- Setup lazy.nvim, most of this is pulled from
-- https://github.com/Integralist/nvim, at least as a starting point
local function merge(t1, t2)
  for i = 1, #t2 do t1[#t1 + 1] = t2[i] end
  return t1
end

local function mappings(client, bufnr)
  -- DISABLED: Because it was not as useful as folke/trouble
  -- vim.api.nvim_create_autocmd({ "BufWrite" }, {
  --   callback = function(_)
  --     local namespace = vim.lsp.diagnostic.get_namespace(client.id)
  --     vim.diagnostic.setqflist({ namespace = namespace })
  --   end
  -- })

  local opts = { noremap = true, silent = true, buffer = bufnr }

  local buf_code_action = "<Cmd>lua vim.lsp.buf.code_action()<CR>"
  local buf_code_action_opts = merge({ desc = "View code actions" }, opts)
  local buf_def = "<Cmd>lua vim.lsp.buf.definition()<CR>"
  local buf_def_split = "<Cmd>sp | lua vim.lsp.buf.definition()<CR>"
  local buf_def_vsplit = "<Cmd>vsp | lua vim.lsp.buf.definition()<CR>"
  local buf_hover = "<Cmd>lua vim.lsp.buf.hover()<CR>"
  local buf_rename = function()
    -- when rename opens the prompt, this autocommand will trigger
    -- it will "press" CTRL-F to enter the command-line window `:h cmdwin`
    -- in this window I can use normal mode keybindings
    local cmdId
    cmdId = vim.api.nvim_create_autocmd({ "CmdlineEnter" }, {
      callback = function()
        local key = vim.api.nvim_replace_termcodes("<C-f>", true, false, true)
        vim.api.nvim_feedkeys(key, "c", false)
        vim.api.nvim_feedkeys("0", "n", false)
        -- autocmd was triggered and so we can remove the ID and return true to delete the autocmd
        cmdId = nil
        return true
      end,
    })
    vim.lsp.buf.rename()
    -- if LPS couldn't trigger rename on the symbol, clear the autocmd
    vim.defer_fn(function()
      -- the cmdId is not nil only if the LSP failed to rename
      if cmdId then
        vim.api.nvim_del_autocmd(cmdId)
      end
    end, 500)
  end
  local buf_rename_opts = merge({ desc = "Rename symbol" }, opts)
  local buf_sig_help = "<Cmd>lua vim.lsp.buf.signature_help()<CR>"
  local buf_sig_help_opts = merge({ desc = "Sig help (cursor over arg)" }, opts)
  local buf_type = "<Cmd>lua vim.lsp.buf.type_definition()<CR>"
  local buf_type_opts = merge({ desc = "Go to 'type' definition" }, opts)
  local diag_show = "<Cmd>lua vim.diagnostic.show()<CR>"
  local diag_show_opts = merge({ desc = "Show project diagnostics" }, opts)

  vim.keymap.set('n', '<c-s>', buf_def_split, opts)
  vim.keymap.set('n', '<c-\\>', buf_def_vsplit, opts)
  vim.keymap.set('n', '<c-]>', buf_def, opts)
  vim.keymap.set('n', ']s', diag_show, diag_show_opts)
  vim.keymap.set('n', 'K', buf_hover, opts)
  vim.keymap.set({"n", "x"}, 'gA', buf_code_action, buf_code_action_opts)
  vim.keymap.set('n', 'gh', buf_sig_help, buf_sig_help_opts)
  vim.keymap.set('n', 'gn', buf_rename, buf_rename_opts)

  if client:supports_method("textDocument/selectionRange") then
    require('lsp-selection-range').setup()
    vim.keymap.set({"n", "x"}, "<leader>[", function() require('lsp-selection-range').selection_range(false) end, merge({ desc = "Expand selection" }, opts))
    vim.keymap.set({"n", "x"}, "<leader>]", function() require('lsp-selection-range').selection_range(true) end, merge({ desc = "Shrink selection" }, opts))
  end

  -- DISABLED: LSP formatting
  --
  -- Looks to be handled by ./lint-and-format.lua stevearc/conform.nvim
  --
  -- if client.supports_method("textDocument/formatting") then
  --   vim.api.nvim_create_autocmd({ "BufWritePre" }, {
  --     group = vim.api.nvim_create_augroup("SharedLspFormatting",
  --       { clear = true }),
  --     pattern = "*",
  --     command = "lua vim.lsp.buf.format()"
  --   })
  -- end

  --if client.server_capabilities.documentSymbolProvider then
  --  require("nvim-navic").attach(client, bufnr)
  --  require("nvim-navbuddy").attach(client, bufnr)
  --  vim.o.winbar = "%{%v:lua.require'nvim-navic'.get_location()%}"
  --end
end

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

        require('solarized').setup({
          on_highlights = function(colors, color)
            diff_fg_base = vim.o.background == 'light' and colors.base00 or colors.base0
            diff_bg_base = vim.o.background == 'light' and colors.base3 or colors.base03
            add_fg = color.blend(colors.git_add, colors.green, 0.2)
            add_bg = color.blend(colors.green, diff_bg_base, 0.1)
            del_inline_fg = color.darken(colors.red, 2)
            del_inline_bg = color.blend(colors.red, diff_bg_base, 0.5)
            del_fg = color.blend(del_inline_fg, diff_fg_base, 0.1)
            del_bg = color.blend(del_inline_bg, diff_bg_base, 0.5)
            return {
              DiffAdd = {
                fg = add_fg,
                bg = add_bg,
              },
              DiffDelete = {
                fg = del_fg,
                bg = del_bg,
              },
              DiffText = {
                fg = color.blend(colors.yellow, diff_bg_base, 0.3),
                bg = color.blend(colors.yellow, colors.green, 0.7),
              },
              NeogitDiffAdd = {
                link = 'DiffAdd',
              },
              NeogitDiffAddCursor = {
                link = 'DiffAdd',
              },
              NeogitDiffAddHighlight = {
                fg = add_fg,
                bg = color.lighten(add_bg, 2),
              },
              NeogitDiffAddInline = {
                fg = color.blend(colors.git_add, diff_fg_base, 0.1),
                bg = color.blend(colors.green, diff_bg_base, 0.5),
              },
              NeogitDiffDelete = {
                fg = del_fg,
                bg = del_bg,
              },
              NeogitDiffDeleteCursor = {
                fg = del_fg,
                bg = del_bg,
              },
              NeogitDiffDeleteHighlight = {
                fg = del_fg,
                bg = color.lighten(del_bg, 2),
              },
              NeogitDiffDeleteInline = {
                fg = del_inline_fg,
                bg = del_inline_bg,
              },
            }
          end,
        })

        vim.cmd.colorscheme 'solarized'
      end,
    },
    { 'tpope/vim-repeat' },
    { 'tpope/vim-surround' },
    { 'tpope/vim-vinegar' },
    {
      "nvim-neo-tree/neo-tree.nvim",
      branch = "v3.x",
      dependencies = {
        "nvim-lua/plenary.nvim",
        "nvim-tree/nvim-web-devicons",
        "MunifTanjim/nui.nvim",
      },
      config = function()
        vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])
        vim.keymap.set("n", "<leader><Tab>", "<Cmd>Neotree toggle reveal<CR>", { desc = "toggle floating file tree" })
        require("neo-tree").setup({
          filesystem = {
            filtered_items = {
              hide_dotfiles = false,
              hide_gitignored = true,
              hide_by_name = {
                ".git",
              },
            },
          },
          reveal = true,
          follow_current_file = {
            enabled = true,
          },
          use_libuv_file_watcher = true,
          window = {
            position = "float",
            mappings = {
              ["s"] = "split_with_window_picker",
              ["v"] = "vsplit_with_window_picker"
            }
          },
        })
      end
    },
    {
      "s1n7ax/nvim-window-picker",
      version = "v2.*",
      config = function()
        local picker = require("window-picker")
        picker.setup({ fg_color = "#000000" })

        vim.keymap.set("n", "<leader><leader>w", function()
          local picked_window_id =
          picker.pick_window() or vim.api.nvim_get_current_win()
          vim.api.nvim_set_current_win(picked_window_id)
        end, { desc = "Pick a window" })
      end
    },
    {
      "nvim-treesitter/nvim-treesitter",
      branch = "main",
      lazy = false,
      build = ":TSUpdate",
      config = function()
        require("nvim-treesitter").install({
          "bash",
          "dockerfile",
          "go",
          "gomod",
          "gowork",
          "html",
          "http",
          "javascript",
          "json",
          "lua",
          "make",
          "markdown",
          "markdown_inline",
          "python",
          "regex",
          "ron",
          "rust",
          "toml",
          "vim",
          "yaml",
        })
      end
    },
    {
      "yousefhadder/markdown-plus.nvim",
      ft = "markdown",
      opts = {},
    },
    {
      "mfussenegger/nvim-dap",
      keys = {
        {
          "<leader>dc",
          function() require('dap').continue() end,
          desc = "dap - start/continue debugging",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader>dx",
          function() require('dap').close() end,
          desc = "dap - stop debugging",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader>do",
          function() require('dap').step_over() end,
          desc = "dap - step over",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader>di",
          function() require('dap').step_into() end,
          desc = "dap - step into",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader>dt",
          function() require('dap').step_out() end,
          desc = "dap - step out",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader>db",
          function() require('dap').toggle_breakpoint() end,
          desc = "dap - toggle breakpoint",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader>dv",
          function() require('dap').set_breakpoint(vim.fn.input('Breakpoint condition: ')) end,
          desc = "dap - set breakpoint condition",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader>dr",
          function() require('dap').repl.open() end,
          desc = "dap - open repl",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader>du",
          function() require('dapui').toggle() end,
          desc = "dap - toggle dap ui",
          mode = "n",
          noremap = true,
          silent = true
        },
      }
    },
    { 
      "mfussenegger/nvim-dap-python",
      ft = { "python" },
      config = function()
        require("dap-python").setup("python")
        for _, c in pairs(require("dap").configurations.python) do
          c["justMyCode"] = false
        end
      end
    },
    {
      "rcarriga/nvim-dap-ui",
      dependencies = {
        "mfussenegger/nvim-dap",
        "nvim-neotest/nvim-nio",
      },
      config = function(_, opts)
        require("dapui").setup(opts)
      end
    },
    {
      "neovim/nvim-lspconfig",
      dependencies = {
        "hrsh7th/nvim-cmp"
      },
      config = function ()
        local client_capabilities = vim.lsp.protocol.make_client_capabilities()
        local cmp_capabilities = require('cmp_nvim_lsp').default_capabilities()
        local capabilities = vim.tbl_deep_extend('force', client_capabilities, cmp_capabilities)

        -- Bash
        vim.lsp.config('bashls', {
          capabilities = capabilities,
          on_attach = function(client, bufnr)
            mappings(client, bufnr)
          end
        })
        vim.lsp.enable('bashls')

        -- Markdown
        vim.lsp.config('markdown_oxide', {
          capabilities = vim.tbl_deep_extend(
            'force',
            capabilities,
            {
              workspace = {
                didChangeWatchedFiles = {
                  dynamicRegistration = true,
                },
              },
            }
          ),
          on_attach = function(client, bufnr)
            mappings(client, bufnr)
          end
        })
        vim.lsp.enable('markdown_oxide')

        vim.lsp.config('marksman', {
          capabilities = capabilities,
          on_attach = function(client, bufnr)
            mappings(client, bufnr)
          end
        })
        vim.lsp.enable('marksman')


        -- Python
        vim.lsp.config('basedpyright', {
          capabilities = capabilities,
          on_attach = function(client, bufnr)
            mappings(client, bufnr)
          end
        })
        vim.lsp.enable('basedpyright')
      end
    },
    {
      "mrcjkb/rustaceanvim",
      version = "^8",
      ft = { "rust" },
      dependencies = {
        "hrsh7th/nvim-cmp"
      },
      config = function()
        vim.g.rustaceanvim = {
          tools = {
            autoSetHints = true,
            inlay_hints = {
              show_parameter_hints = true,
              parameter_hints_prefix = "in: ", -- "<- "
              other_hints_prefix = "out: "     -- "=> "
            }
          },
          -- LSP configuration
          --
          -- REFERENCE:
          -- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
          -- https://rust-analyzer.github.io/manual.html#configuration
          -- https://rust-analyzer.github.io/manual.html#features
          --
          -- NOTE: The configuration format is `rust-analyzer.<section>.<property>`.
          --       <section> should be an object.
          --       <property> should be a primitive.
          server = {
            on_attach = function(client, bufnr)
              mappings(client, bufnr)

              local bufopts = {
                noremap = true,
                silent = true,
                buffer = bufnr
              }
              vim.keymap.set('n', '<leader><leader>rr',
              "<Cmd>RustLsp runnables<CR>", bufopts)
              vim.keymap.set({'n', 'x'}, 'K',
              "<Cmd>RustLsp hover actions<CR>", bufopts)
            end,
            settings = {
              -- rust-analyzer language server configuration
              ['rust-analyzer'] = {
                assist = {
                  preferSelf = true,
                },
                cargo = {
                  features = "all",
                },
                check = {
                  -- default: `cargo check`
                  command = "clippy",
                  features = "all",
                },
                completion = {
                  privateEditable = {
                    enable = true,
                  },
                },
                inlayHints = {
                  discriminantHints = {
                    enable = true,
                  },
                  lifetimeElisionHints = {
                    enable = true,
                    useParameterNames = true
                  },
                  parameterHints = {
                    missingArguments = {
                      enable = true,
                    }
                  },
                },
                lens = {
                  references = {
                    adt = {
                      enable = true,
                    },
                    enumVariant = {
                      enable = true,
                    },
                    method = {
                      enable = true,
                    },
                    trait = {
                      enable = true,
                    },
                  },
                },
              }
            }
          }
        }
      end
    },
    {
      "saecki/crates.nvim",
      event = { "BufRead Cargo.toml" },
      config = function()
        require("crates").setup({
          lsp = {
            enabled = true,
            on_attach = function(client, bufnr)
              mappings(client, bufnr)
            end,
            actions = true,
            completion = true,
            hover = true
          },
          completion = {
            crates = {
              enabled = true,
              max_results = 8,
              min_chars = 3,
            },
          },
        })
      end,
    },
    {
      "hrsh7th/nvim-cmp",
      dependencies = {
        "onsails/lspkind.nvim",
        "PhilRunninger/cmp-rpncalc",
      },
      config = function()
        local cmp = require("cmp")
        local lspkind = require("lspkind")
        cmp.setup({
          experimental = { ghost_text = true },
          window = {
            completion = cmp.config.window.bordered(),
            documentation = cmp.config.window.bordered()
          },
          formatting = {
            format = lspkind.cmp_format(),
          },
          mapping = {
            ["<Up>"] = cmp.mapping.select_prev_item(),
            ["<Down>"] = cmp.mapping.select_next_item(),
            ["<Left>"] = cmp.mapping.select_prev_item(),
            ["<Right>"] = cmp.mapping.select_next_item(),
            ["<C-b>"] = cmp.mapping.scroll_docs(-4),
            ["<C-f>"] = cmp.mapping.scroll_docs(4),
            ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
            ["<C-e>"] = cmp.mapping.close(),
            ["<Tab>"] = cmp.mapping.confirm({
              behavior = cmp.ConfirmBehavior.Insert,
              select = true
            })
          },
          sources = cmp.config.sources(
            {
              { name = "nvim_lsp", keyword_length = 1 },
              { name = "nvim_lsp_signature_help" },
              { name = "path" },
            },
            {
              { name = 'buffer' },
            },
            {
              { name = 'rpncalc' },
            }
          )
        })

        cmp.setup.cmdline({ "/", "?" }, {
          mapping = cmp.mapping.preset.cmdline(), -- Tab for selection (arrows needed for selecting past items)
          sources = { { name = "buffer" } }
        })

        cmp.setup.cmdline({ ":" }, {
          mapping = cmp.mapping.preset.cmdline(), -- Tab for selection (arrows needed for selecting past items)
          sources = { { name = "cmdline" }, { name = "path" } }
        })
      end
    },
    { "hrsh7th/cmp-nvim-lsp" },
    { "hrsh7th/cmp-nvim-lsp-signature-help" },
    { "hrsh7th/cmp-buffer" },
    { "hrsh7th/cmp-cmdline" },
    { "hrsh7th/cmp-path" },
    {
      "RRethy/vim-illuminate",
      config = function()
        require('illuminate').configure({})
        -- change the highlight style
        vim.api.nvim_set_hl(0, "IlluminatedWordText", { link = "Visual" })
        vim.api.nvim_set_hl(0, "IlluminatedWordRead", { link = "Visual" })
        vim.api.nvim_set_hl(0, "IlluminatedWordWrite", { link = "Visual" })
        vim.api.nvim_create_autocmd({ "ColorScheme" }, {
          pattern = { "*" },
          callback = function(ev)
            vim.api.nvim_set_hl(0, "IlluminatedWordText", { link = "Visual" })
            vim.api.nvim_set_hl(0, "IlluminatedWordRead", { link = "Visual" })
            vim.api.nvim_set_hl(0, "IlluminatedWordWrite", { link = "Visual" })
          end
        })
      end,
    },
    {
      "lewis6991/gitsigns.nvim",
      config = function()
        require('gitsigns').setup({
          signcolumn = false,
          numhl = true,
          on_attach = function(bufnr)
            if vim.b.loaded_codediff == nil then
              local gitsigns = require('gitsigns')
              local function map(mode, l, r, opts)
                opts = opts or {}
                opts.buffer = bufnr
                vim.keymap.set(mode, l, r, opts)
              end
              map('n', ']c', function()
                if vim.wo.diff then
                  vim.cmd.normal({']c', bang = true})
                else
                  gitsigns.nav_hunk('next')
                end
              end)

              map('n', '[c', function()
                if vim.wo.diff then
                  vim.cmd.normal({'[c', bang = true})
                else
                  gitsigns.nav_hunk('prev')
                end
              end)

              map('n', 'z', "<Cmd>Gitsigns<CR>")
            end
          end,
        })
      end
    },
    {
      "folke/snacks.nvim",
      priority = 1000,
      lazy = false,
      ---@type snacks.Config
      opts = {
        picker = {
          jump = {
            jumplist = false,
          },
          matcher = {
            frecency = true,
          },
          sources = {
            buffers = {
              layout = "select",
            },
            explorer = {
              auto_close = true,
              layout = {
                hidden = { "preview" },
                layout = {
                  backdrop = false,
                  width = 0.8,
                  min_width = 80,
                  max_width = 100,
                  height = 0.8,
                  min_height = 2,
                  box = "vertical",
                  border = true,
                  title = "{title}",
                  title_pos = "center",
                  { win = "input", height = 1, border = "bottom" },
                  { win = "list", border = "none" },
                },
              },
            },
            files = {
              layout = "select",
            },
            keymaps = {
              layout = "vertical"
            },
            lsp_definitions = {
              layout = "dropdown",
            },
            lsp_declarations = {
              layout = "dropdown",
            },
            lsp_implementations = {
              layout = "dropdown",
            },
            lsp_incoming_calls = {
              layout = "dropdown",
            },
            lsp_outgoing_calls = {
              layout = "dropdown",
            },
            lsp_references = {
              layout = "dropdown",
            },
            lsp_symbols = {
              layout = "dropdown",
            },
            lsp_workspace_symbols = {
              layout = "dropdown",
            },
            lsp_type_definitions = {
              layout = "dropdown",
            },
          },
        },
      },
      keys = {
        { "<leader>fb", function() Snacks.picker.buffers() end, desc = "Select buffer" },
        { "<leader>fe", function() Snacks.picker.explorer() end, desc = "Explore files" },
        { "<leader>ff", function() Snacks.picker.files() end, desc = "Find file" },
        { "gd", function() Snacks.picker.lsp_definitions() end, desc = "LSP Goto Definition" },
        { "gD", function() Snacks.picker.lsp_declarations() end, desc = "LSP Goto Declaration" },
        { "gr", function() Snacks.picker.lsp_references() end, nowait = true, desc = "LSP References" },
        { "gI", function() Snacks.picker.lsp_implementations() end, desc = "LSP Goto Implementation" },
        { "gy", function() Snacks.picker.lsp_type_definitions() end, desc = "LSP Goto T[y]pe Definition" },
        { "gai", function() Snacks.picker.lsp_incoming_calls() end, desc = "LSP C[a]lls [I]ncoming" },
        { "gao", function() Snacks.picker.lsp_outgoing_calls() end, desc = "LSP C[a]lls [O]utgoing" },
        { '<leader>s"', function() Snacks.picker.registers() end, desc = "Registers" },
        { "<leader>sb", function() Snacks.picker.lines() end, desc = "Buffer Lines" },
        { "<leader>sB", function() Snacks.picker.grep_buffers() end, desc = "Grep Open Buffers" },
        { "<leader>sd", function() Snacks.picker.diagnostics() end, desc = "Diagnostics" },
        { "<leader>sD", function() Snacks.picker.diagnostics_buffer() end, desc = "Buffer Diagnostics" },
        { "<leader>sg", function() Snacks.picker.grep() end, desc = "Grep" },
        { "<leader>sj", function() Snacks.picker.jumps() end, desc = "Jumps" },
        { "<leader>sm", function() Snacks.picker.marks() end, desc = "Marks" },
        { "<leader>sk", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
        { "<leader>sq", function() Snacks.picker.qflist() end, desc = "Quickfix List" },
        { "<leader>sR", function() Snacks.picker.resume() end, desc = "Resume" },
        { "<leader>su", function() Snacks.picker.undo() end, desc = "Undo History" },
        { "<leader>ss", function() Snacks.picker.lsp_symbols() end, desc = "LSP Symbols" },
        { "<leader>sS", function() Snacks.picker.lsp_workspace_symbols() end, desc = "LSP Workspace Symbols" },
        { "<leader>st", function() Snacks.picker.treesitter() end, desc = "Treesitter" },
        { "<leader>sw", function() Snacks.picker.grep_word() end, desc = "Visual selection or word", mode = { "n", "x" }},
      },
    },
      {
        'MagicDuck/grug-far.nvim',
        config = function()
          require('grug-far').setup({});
          vim.keymap.set("n", "<leader>S", "<Cmd>GrugFar<CR>", { desc = "search and replace" })
        end
      },
      {
        "NeogitOrg/neogit",
        dependencies = {
          "nvim-lua/plenary.nvim",
          "esmuellert/codediff.nvim",
        },
        keys = {
          {
            "<leader>gg",
            "<cmd>Neogit<cr>",
            desc = "Neogit",
            mode = "n",
            noremap = true,
            silent = true
          },
          {
            "<leader>gl",
            "<cmd>lua require('neogit').action('log', 'log_current')()<cr>",
            desc = "Neogit log current branch",
            mode = "n",
            noremap = true,
            silent = true
          },
          {
            "<leader>gf",
            function()
              local file = vim.fn.expand('%')
              require('neogit').action('log', 'log_current', { '--', file })()
            end,
            desc = "Neogit log current file",
            mode = "n",
            noremap = true,
            silent = true
          },
        }
      },
      {
        "towolf/vim-helm",
      },
      { "pmizio/typescript-tools.nvim",
        dependencies = {
          "nvim-lua/plenary.nvim",
          "neovim/nvim-lspconfig",
        },
        opts = {},
        config = function()
          require("typescript-tools").setup({});
        end
      },
      {
        url = "https://codeberg.org/andyg/leap.nvim",
        config = function()
          vim.keymap.set({"n", "x", "o"}, "s" , "<Plug>(leap-forward)"      , { desc = "leap forward" }      )
          vim.keymap.set({"n", "x", "o"}, "S" , "<Plug>(leap-backward)"     , { desc = "leap backward" }     )
          vim.keymap.set({"n",         }, "gs", "<Plug>(leap-from-window)"  , { desc = "leap from window" }  )
          vim.keymap.set({"x", "o"     }, "x" , "<Plug>(leap-forward-till)" , { desc = "leap forward till" } )
          vim.keymap.set({"x", "o"     }, "X" , "<Plug>(leap-backward-till)", { desc = "leap backward till" })
        end,
      },
      {
        'stevearc/conform.nvim',
        config = function()
          require("conform").setup({
            formatters_by_ft = {
              python = { "ruff_format" },
              rust = { "rustfmt", lsp_format = "fallback" },
            },
            format_after_save = function(bufnr)
              -- disable with a global or buffer-local variable
              if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
                return
              end
              return { timeout_ms = 5000, lsp_format = "fallback" }
            end,
          })

          require("conform").formatters.rustfmt = {
            options = {
              default_edition = "2024",
              nightly = true,
            },
          }

          vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"

          vim.api.nvim_create_user_command(
            "FormatDisable",
            function(args)
              if args.bang then
                -- FormatDisable! will disable formatting just for this buffer
                vim.b.disable_autoformat = true
              else
                vim.g.disable_autoformat = true
              end
            end,
            {
              desc = "Disable autoformat-on-save",
              bang = true,
            }
          )

          vim.api.nvim_create_user_command(
            "FormatEnable",
            function()
              vim.b.disable_autoformat = false
              vim.g.disable_autoformat = false
            end,
            {
              desc = "Re-enable autoformat-on-save",
            }
          )

          vim.keymap.set("n", "<leader><leader>fi", "<Cmd>ConformInfo<CR>", { desc = "Show Conform log" })
          vim.keymap.set("n", "<leader><leader>fd", "<Cmd>FormatDisable<CR>", { desc = "Disable autoformat-on-save" })
          vim.keymap.set("n", "<leader><leader>fe", "<Cmd>FormatEnable<CR>", { desc = "Re-enable autoformat-on-save" })
        end,
      },
      {
        "junegunn/fzf",
      },
      {
        "junegunn/fzf.vim",
        dependencies = {
          "junegunn/fzf",
        },
      },
      {
        "kevinhwang91/nvim-bqf",
        dependencies = {
          "junegunn/fzf.vim",
        },
        ft = "qf",
      },
      {
        "folke/todo-comments.nvim",
        dependencies = {
          "nvim-lua/plenary.nvim"
        },
        opts = {
          signs = false,
        },
      },
      {
        "tpope/vim-dadbod"
      },
      {
        "echasnovski/mini.ai",
        version = false,
        config = function()
          require('mini.ai').setup({
            custom_textobjects = {
              x = { ': ().*(),' }
            }
          })
        end
      },
      {
        "stevearc/aerial.nvim",
        dependencies = {
          "nvim-treesitter/nvim-treesitter",
          "nvim-tree/nvim-web-devicons"
        },
        config = function()
          require("aerial").setup({
            -- optionally use on_attach to set keymaps when aerial has attached to a buffer
            on_attach = function(bufnr)
              -- Jump forwards/backwards with '{' and '}'
              vim.keymap.set("n", "{", "<cmd>AerialPrev<CR>", { buffer = bufnr })
              vim.keymap.set("n", "}", "<cmd>AerialNext<CR>", { buffer = bufnr })
              vim.cmd("highlight! default link AerialLine CursorLineNr")
            end,
          })
          -- You probably also want to set a keymap to toggle aerial
          vim.keymap.set("n", "<leader>a", "<cmd>AerialToggle!<CR>")
        end
      },
      {
        "tpope/vim-abolish",
      },
      {
        "folke/trouble.nvim",
        opts = {},
        cmd = "Trouble",
      },
      {
        "yorickpeterse/nvim-pqf",
        config = function ()
          require('pqf').setup()
        end,
      },
    {
      'TrevorS/uuid-nvim',
      config = function()
        local uuid = require('uuid-nvim')
        uuid.setup({
          case = 'lower',
          insert = 'before',
          quotes = 'none',
        })

        vim.keymap.set('i', '<M-u>', uuid.insert_v4)
      end
    },
    {
      "gbprod/yanky.nvim",
      opts = {
        ring = {
          storage = "memory",
        },
        highlight = {
          on_put = true,
          on_yank = true,
          timer = 100,
        },
      },
    },
    {
      "stevearc/quicker.nvim",
      ft = "qf",
    },
    {
      "esmuellert/codediff.nvim",
      cmd = "CodeDiff",
      opts = {
        highlights = {
          char_insert = 'NeogitDiffAddInline',
          char_delete = 'NeogitDiffDeleteInline',
        },
      },
      config = function()
        vim.api.nvim_create_autocmd("User", {
          pattern = "CodeDiffOpen",
          callback = function()
            vim.b.loaded_codediff = true
          end,
        })
      end,
    },
    {
      "carderne/pi-nvim",
      config = function()
        require("pi-nvim").setup()
      end,
    },
    {
      "retran/meow.yarn.nvim",
      dependencies = { "MunifTanjim/nui.nvim" },
      config = function()
        require("meow.yarn").setup({
          mappings = {
            quit = "<esc>",
          },
          render_node = function(node)
            local function one_line(s)
              local text = tostring(s or ""):gsub("[\r\n]+", " ↩ ")
              return text
            end

            local out = {
              string.rep("  ", math.max(0, node.depth - 1)),
              node.icon or "",
              " ",
              one_line(node.name),
            }

            if node.file then
              table.insert(out, ("  (%s:%s)"):format(one_line(node.file), node.line or "?"))
            end

            return table.concat(out)
          end,
        })

        -- TODO: remove when meow.yarn.nvim switches from deprecated
        -- `client.request(...)` to `client:request(...)` for Nvim 0.13.
        local util = require("meow.yarn.util")
        util.lsp.request_async = function(client, method, params, bufnr, callback)
          client:request(method, params or {}, function(err, res)
            if err then
              if err.code == -32800 then
                return callback(nil)
              end
              vim.schedule(function()
                vim.notify(
                  string.format("LSP %s: %s", method, err.message or "error"),
                  vim.log.levels.WARN
                )
              end)
              return callback(nil)
            end
            callback(res or {})
          end, bufnr or 0)
        end

        vim.keymap.set("n", "<leader>yt", function() require("meow.yarn").open_tree("type_hierarchy", "supertypes") end, { desc = "Yarn: Type Hierarchy (Super)" })
        vim.keymap.set("n", "<leader>yT", function() require("meow.yarn").open_tree("type_hierarchy", "subtypes") end, { desc = "Yarn: Type Hierarchy (Sub)" })
        vim.keymap.set("n", "<leader>yc", function() require("meow.yarn").open_tree("call_hierarchy", "callers") end, { desc = "Yarn: Call Hierarchy (Callers)" })
        vim.keymap.set("n", "<leader>yC", function() require("meow.yarn").open_tree("call_hierarchy", "callees") end, { desc = "Yarn: Call Hierarchy (Callees)" })
      end,
    },
  },
  install = { colorscheme = { "solarized" } },
})
