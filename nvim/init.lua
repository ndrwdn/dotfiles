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

-- Shortuct to clear current search
vim.keymap.set('n', '', ':nohlsearch<cr>', { silent = true })

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
  local buf_doc_sym = "<Cmd>lua vim.lsp.buf.document_symbol()<CR>"
  local buf_doc_sym_opts = merge({ desc = "List doc symbols in qf win" }, opts)
  local buf_hover = "<Cmd>lua vim.lsp.buf.hover()<CR>"
  local buf_impl = "<Cmd>lua vim.lsp.buf.implementation()<CR>"
  local buf_impl_opts = merge({ desc = "List all implementations" }, opts)
  local buf_incoming_calls = "<Cmd>lua vim.lsp.buf.incoming_calls()<CR>"
  local buf_incoming_calls_opts = merge({ desc = "List all callers" }, opts)
  local buf_project = "<Cmd>lua vim.lsp.buf.workspace_symbol()<CR>"
  local buf_project_opts = merge({ desc = "Search project-wide symbols" }, opts)
  local buf_ref = "<Cmd>lua vim.lsp.buf.references()<CR>"
  local buf_ref_opts = merge({ desc = "List all references" }, opts)
  local buf_rename = "<Cmd>lua vim.lsp.buf.rename()<CR>"
  local buf_rename_opts = merge({ desc = "Rename symbol" }, opts)
  local buf_sig_help = "<Cmd>lua vim.lsp.buf.signature_help()<CR>"
  local buf_sig_help_opts = merge({ desc = "Sig help (cursor over arg)" }, opts)
  local buf_type = "<Cmd>lua vim.lsp.buf.type_definition()<CR>"
  local buf_type_opts = merge({ desc = "Go to 'type' definition" }, opts)
  local diag_next = "<Cmd>lua vim.diagnostic.goto_next()<CR>"
  local diag_next_opts = merge({ desc = "Go to next diagnostic" }, opts)
  local diag_open_float = "<Cmd>lua vim.diagnostic.open_float()<CR>"
  local diag_open_float_opts = merge({ desc = "Float current diag" }, opts)
  local diag_prev = "<Cmd>lua vim.diagnostic.goto_prev()<CR>"
  local diag_prev_opts = merge({ desc = "Go to prev diagnostic" }, opts)
  local diag_show = "<Cmd>lua vim.diagnostic.show()<CR>"
  local diag_show_opts = merge({ desc = "Show project diagnostics" }, opts)

  vim.keymap.set('n', '<c-s>', buf_def_split, opts)
  vim.keymap.set('n', '<c-\\>', buf_def_vsplit, opts)
  vim.keymap.set('n', '<c-]>', buf_def, opts)
  vim.keymap.set('n', '[x', diag_prev, diag_prev_opts)
  vim.keymap.set('n', ']r', diag_open_float, diag_open_float_opts)
  vim.keymap.set('n', ']s', diag_show, diag_show_opts)
  vim.keymap.set('n', ']x', diag_next, diag_next_opts)
  vim.keymap.set('n', 'K', buf_hover, opts)
  vim.keymap.set('n', 'ga', buf_code_action, buf_code_action_opts)
  vim.keymap.set('n', 'gi', buf_incoming_calls, buf_incoming_calls_opts)
  vim.keymap.set('n', 'gd', buf_doc_sym, buf_doc_sym_opts)
  vim.keymap.set('n', 'gh', buf_sig_help, buf_sig_help_opts)
  vim.keymap.set('n', 'gm', buf_impl, buf_impl_opts)
  vim.keymap.set('n', 'gn', buf_rename, buf_rename_opts)
  vim.keymap.set('n', 'gp', buf_project, buf_project_opts)
  vim.keymap.set('n', 'gr', buf_ref, buf_ref_opts)
  vim.keymap.set('n', 'gy', buf_type, buf_type_opts)

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
      version = "v1.*",
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
      build = ":TSUpdate",
      config = function()
        require("nvim-treesitter.configs").setup({
          ensure_installed = {
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
            "python",
            "regex",
            "rust",
            "toml",
            "vim",
            "yaml",
          },
          highlight = { enable = true },
        })
      end
    },
    {
      "tadmccorkle/markdown.nvim",
      ft = "markdown",
      config = function ()
        require("markdown").setup({
          mappings = {
            link_follow = "gx"
          }
        })
      end
    },
    {
      "mfussenegger/nvim-dap",
      keys = {
        {
          "<leader><leader>bc",
          function() require('dap').continue() end,
          desc = "start debugging",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader><leader>bx",
          function() require('dap').close() end,
          desc = "stop debugging",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader><leader>bo",
          function() require('dap').step_over() end,
          desc = "step over",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader><leader>bi",
          function() require('dap').step_into() end,
          desc = "step into",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader><leader>bt",
          function() require('dap').step_out() end,
          desc = "step out",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader><leader>bb",
          function() require('dap').toggle_breakpoint() end,
          desc = "toggle breakpoint",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader><leader>bv",
          function() require('dap').set_breakpoint(vim.fn.input('Breakpoint condition: ')) end,
          desc = "set breakpoint condition",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader><leader>br",
          function() require('dap').repl.open() end,
          desc = "open repl",
          mode = "n",
          noremap = true,
          silent = true
        },
        {
          "<leader><leader>bu",
          function() require('dapui').toggle() end,
          desc = "toggle dap ui",
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
      config = function ()
        require'lspconfig'.basedpyright.setup({
          on_attach = function(client, bufnr)
            mappings(client, bufnr)
            require("illuminate").on_attach(client)
          end
        })
      end
    },
    {
      "mrcjkb/rustaceanvim",
      version = "^4",
      ft = { "rust" },
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
              require("illuminate").on_attach(client)

              local bufopts = {
                noremap = true,
                silent = true,
                buffer = bufnr
              }
              vim.keymap.set('n', '<leader><leader>rr',
              "<Cmd>RustLsp runnables<CR>", bufopts)
              vim.keymap.set('n', 'K',
              "<Cmd>RustLsp hover actions<CR>", bufopts)
            end,
            settings = {
              -- rust-analyzer language server configuration
              ['rust-analyzer'] = {
                assist = {
                  importEnforceGranularity = true,
                  importPrefix = "create"
                },
                cargo = { allFeatures = true },
                checkOnSave = {
                  -- default: `cargo check`
                  command = "clippy",
                  allFeatures = true
                },
                inlayHints = {
                  lifetimeElisionHints = {
                    enable = true,
                    useParameterNames = true
                  }
                }
              }
            }
          }
        }
      end
    },
    {
      "hrsh7th/nvim-cmp",
      config = function()
        local cmp = require("cmp")
        cmp.setup({
          experimental = { ghost_text = true },
          window = {
            completion = cmp.config.window.bordered(),
            documentation = cmp.config.window.bordered()
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
    { "RRethy/vim-illuminate" },
    {
      "lewis6991/gitsigns.nvim",
      config = function()
        require('gitsigns').setup({
          signcolumn = false,
          numhl = true,
          on_attach = function(bufnr)
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
          end,
        })
      end
    },
    {
      "nvim-telescope/telescope.nvim",
      dependencies = {
        "nvim-lua/plenary.nvim",
        "debugloop/telescope-undo.nvim",
        "nvim-telescope/telescope-live-grep-args.nvim",
        "jmacadie/telescope-hierarchy.nvim",
        "kkharji/sqlite.lua",
        "nvim-telescope/telescope-smart-history.nvim",
        "jonarrien/telescope-cmdline.nvim",
        "OliverChao/telescope-picker-list.nvim",
        "nvim-telescope/telescope-frecency.nvim",
      },
      config = function()
        --[[
            NOTE: Scroll the preview window using <C-d> and <C-u>.

            Opening multiple files isn't a built-in feature of Telescope.
            We can't get that behaviour without a lot of extra config.
            The best we can do is send results to the quickfix window.

            There are two different ways of approaching this:

            1. Filtering files using a search pattern.
            2. Manually selecting files.

            For first scenario just type a search pattern (which will filter the
            list), then press `<C-q>` to send filtered list to quickfix window.

            For the second scenario you would need to:

            - Select multiple files using <Tab>
            - Send the selected files to the quickfix window using <C-o>
            - Search the quickfix window (using either :copen or <leader>q)

            The first approach is fine for simple cases. For example, you want to
            open all Markdown files. You just type `.md` and then `<C-q>`. But if
            you wanted to open specific Markdown files like README.md and HELP.md
            then you'd need the second approach which gives finer-grain control.
          ]]
          local actions = require("telescope.actions")
          local ts = require("telescope")
          local lga_actions = require("telescope-live-grep-args.actions")

          ts.setup({
            defaults = {
              layout_strategy = "vertical",
              layout_config = { height = 0.75 },
              file_ignore_patterns = {
                ".git/",
              },
              mappings = {
                i = {
                  ["<esc>"] = actions.close,
                  ["<C-o>"] = actions.send_selected_to_qflist,
                  ["<C-Down>"] = actions.cycle_history_next,
                  ["<C-Up>"] = actions.cycle_history_prev,
                }
              },
              scroll_strategy = "limit",
              vimgrep_arguments = {
                'rg',
                '--color=never',
                '--no-heading',
                '--with-filename',
                '--line-number',
                '--column',
                '--smart-case',
                '--hidden'
              },
              history = {
                path = '~/.local/share/nvim/telescope_history.sqlite3',
                limit = 1000,
              },
              cache_picker = {
                num_pickers = 5,
                limit_entries = 500,
                ignore_empty_prompt = true,
              },
            },
            extensions = {
              heading = {
                treesitter = true
              },
              live_grep_args = {
                auto_quoting = true,
                mappings = {
                  i = {
                    ["<C-y>"] = lga_actions.quote_prompt(),
                    ["<C-i>"] = lga_actions.quote_prompt({ postfix = " --iglob " }),
                    ["<C-space>"] = actions.to_fuzzy_refine,
                  },
                },
              },
              frecency = {
                enabled_prompt_mappings = true,
              },
            }
          })

          ts.load_extension("fzf")
          ts.load_extension("jsonfly")
          ts.load_extension("ui-select")
          ts.load_extension("undo")
          ts.load_extension("live_grep_args")
          ts.load_extension("hierarchy")
          ts.load_extension("smart_history")
          ts.load_extension("cmdline")
          ts.load_extension("frecency")
          ts.load_extension("picker_list") -- must always be last

          vim.keymap.set("n", "<leader>b", "<Cmd>Telescope buffers sort_lastused=true sort_mru=true<CR>", { desc = "search buffers" })
          vim.keymap.set("n", "<leader>e", "<Cmd>Telescope commands<CR>", { desc = "search Ex commands" })
          vim.keymap.set("n", "<leader>f", "<Cmd>Telescope frecency workspace=CWD<CR>", { desc = "search files using frecency" })
          vim.keymap.set("v", "<leader>f", function() require('telescope').extensions.frecency.frecency({ workspace= 'CWD', default_text = table.concat(get_visual_selection(), ' ') }) end, { desc = "search files using frecency (visual, pre-populate)" })
          vim.keymap.set("n", "<leader>F", "<Cmd>Telescope find_files hidden=true<CR>", { desc = "search files using find files" })
          vim.keymap.set("n", "<leader>h", "<Cmd>Telescope help_tags<CR>", { desc = "search help" })
          vim.keymap.set("n", "<leader>i", "<Cmd>Telescope builtin<CR>", { desc = "search builtins" })
          vim.keymap.set("n", "<leader>j", "<Cmd>Telescope jsonfly<CR>", { desc = "search current JSON structure" })
          vim.keymap.set("n", "<leader>k", "<Cmd>Telescope keymaps<CR>", { desc = "search key mappings" })
          vim.keymap.set("n", "<leader>ld", "<Cmd>Telescope diagnostics<CR>", { desc = "search lsp diagnostics" })
          vim.keymap.set("n", "<leader>li", "<Cmd>Telescope lsp_incoming_calls<CR>", { desc = "search lsp incoming calls" })
          vim.keymap.set("n", "<leader>lo", "<Cmd>Telescope lsp_outgoing_calls<CR>", { desc = "search lsp outgoing calls" })
          vim.keymap.set("n", "<leader>lr", "<Cmd>Telescope lsp_references<CR>", { desc = "search lsp code reference" })
          vim.keymap.set("n", "<leader>ls", "<Cmd>lua require('telescope.builtin').lsp_document_symbols({show_line = true})<CR>", { desc = "search lsp document tree" })
          vim.keymap.set("n", "<leader>q", "<Cmd>Telescope quickfix<CR>", { desc = "search quickfix list" })
          vim.keymap.set("n", "<leader>r", "<Cmd>Telescope current_buffer_fuzzy_find<CR>", { desc = "search current buffer text" })
          vim.keymap.set("n", "<leader>s", "<Cmd>Telescope treesitter<CR>", { desc = "search treesitter symbols" }) -- similar to lsp_document_symbols but treesitter doesn't know what a 'struct' is, just that it's a 'type'.
          vim.keymap.set("n", "<leader>x", ":lua require('telescope').extensions.live_grep_args.live_grep_args()<CR>", { desc = "search text" })
          vim.keymap.set("v", "<leader>x", function() require('telescope').extensions.live_grep_args.live_grep_args({ default_text = table.concat(get_visual_selection(), ' ') }) end, { desc = "search selected text" })
          vim.keymap.set("n", "<leader>u", "<Cmd>Telescope undo<CR>", { desc = "undo" })
          vim.keymap.set("n", "<leader>t", "<Cmd>Telescope resume<CR>", { desc = "resume last telescope" })
          vim.keymap.set("n", "<leader>T", "<Cmd>Telescope pickers<CR>", { desc = "select from previous telescopes pickers" })
          vim.keymap.set("n", "Q", "<Cmd>Telescope cmdline<CR>", { noremap = true, desc = "telescope command line" })
          vim.keymap.set("n", "<leader>pl", "<Cmd>Telescope picker_list<CR>", { desc = "list telescope pickers" })

          vim.api.nvim_create_autocmd("FileType", {
            pattern = "TelescopeResults",
            callback = function()
              vim.cmd([[setlocal nofoldenable]])
              vim.api.nvim_set_hl(0, "TelescopePromptCounter", { link = "TelescopePromptPrefix" })
            end
          })
        end
      },
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make"
      },
      {
        "noahbald/grit-telescope.nvim",
        -- NOTE: Keys are not provided by default
        keys = {
          { "<leader>fq", "<cmd>Telescope grit query<cr>", desc = "Telescope Grit Query" },
          { "<leader>fQ", "<cmd>Telescope grit list<cr>", desc = "Telescope Grit User Patterns"},
        },
      },
      { "Myzel394/jsonfly.nvim" },
      {
        "nvim-telescope/telescope-ui-select.nvim",
        config = function() require("telescope").setup({}) end
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
          "sindrets/diffview.nvim",
          "nvim-telescope/telescope.nvim",
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
        "ggandor/leap.nvim",
        config = function()
          require("leap").add_default_mappings(true)
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
          require('mini.ai').setup()
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
  },
  install = { colorscheme = { "solarized" } },
})
