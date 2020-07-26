" General settings
"""""""""""""""""""""""""""""""""""""
" Don't use swapfiles
set noswapfile

" Searching
set incsearch
set hlsearch
set ignorecase
set smartcase

" Display settings
" 'c' shows '$' at end of text.
set cpoptions+=$

" Vertical splits for diffing
set diffopt+=vertical

" Don't use fill characters for splits, folds, etc.
set fillchars=""

" No menubar, toolbar, or scollbar in GUI mode
set guioptions-=mTr

" Always show the status line.
set laststatus=2

" Show line numbers
set number

" Open new windows to the bottom/right instead of top/left
set splitbelow
set splitright

" Don't show intro message
set shortmess+=I

" Use syntax highlighting
syntax enable

" Statusline and window title
set statusline=%t\ %3(%m%)%4(%r%)%y\ [%{&ff}]%=Line:\ %-10(%l/%L%)\ Col:\ %-4(%c%)\ Buf:\ %-2(%n%)\ %11([%3b][0x%02B]%)
set titlestring=%t\ %m%(\ (%{expand(\"%:p:~:h\")})%)

" Show partially complete commands (useful for Normal mode).
set showcmd

" Mouse settings
set nomousehide
set mouse=a

" Allow for hidden buffers
set hidden

" Faster display
set timeoutlen=500

" Indentation settings
set expandtab
set shiftround
set smartindent
set shiftwidth=2
set softtabstop=2
set tabstop=2

" Allow backspacing over anything
set backspace=indent,eol,start


" Keybindings
"""""""""""""""""""""""""""""""""""""
" Space is our leader, must be set ahead of keybindings using it
let mapleader=" "
let maplocalleader=" "

" Make Shift-Tab unindent in insert mode
inoremap <S-Tab> <C-d>


" Plugins and plugin-specific config
"""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

Plug 'altercation/vim-colors-solarized'

Plug 'rust-lang/rust.vim', { 'for': ['rust'] }
"Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh', 'for': ['rust'] }
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }

Plug 'junegunn/fzf' " Using this with symlinks to skim

Plug 'rkitover/vimpager'

Plug 'rbgrouleff/bclose.vim'
Plug 'francoiscabrol/ranger.vim'

"Plug 'ludovicchabant/vim-gutentags'

Plug 'tpope/vim-speeddating'
Plug 'vim-scripts/utl.vim'
Plug 'jceb/vim-orgmode'

" For Lisps
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'frazrepo/vim-rainbow'
Plug 'jpalardy/vim-slime'

Plug 'wlangstroth/vim-racket'

call plug#end()

" Plugin config that must be specified after Plugged has finished

" Solarized colorscheme
set background=dark
silent! colorscheme solarized

" Language server config
let g:LanguageClient_serverCommands = {
  \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
  \ }

augroup LanguageClient_config
  autocmd!
  autocmd User LanguageClientStarted nmap <leader>d :call LanguageClient#textDocument_definition()<CR>
  autocmd User LanguageClientStarted nmap <leader>r :call LanguageClient#textDocument_rename()<CR>
  autocmd User LanguageClientStarted nmap <leader>h :call LanguageClient#textDocument_hover()<CR>
augroup END

" Ranger config
let g:ranger_replace_netrw = 1

" Rainbow config
let g:rainbow_active = 1

" For OrgMode files
autocmd FileType org setlocal foldlevel=99

" Fix indentation for yaml files when commenting out a section
autocmd FileType yaml setlocal expandtab indentkeys-=0#

" Slime config
let g:slime_target = "vimterminal"
let g:slime_vimterminal_cmd = "racket"
let g:slime_vimterminal_config = {"vertical": 1, "term_finish": "close"}
