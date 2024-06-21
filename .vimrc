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

" Use indentation from plugins
filetype plugin indent on

" Statusline and window title
set statusline=%t\ %3(%m%)%4(%r%)%y\ [%{&ff}]%=Line:\ %-10(%l/%L%)\ Col:\ %-4(%c%)\ Buf:\ %-2(%n%)\ %11([%3b][0x%02B]%)
set titlestring=%t\ %m%(\ (%{expand(\"%:p:~:h\")})%)

" Show partially complete commands (useful for Normal mode).
set showcmd

" Command-line auto-completion
set wildmenu
set wildoptions=pum
set wildmode=full:lastused

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

" Formatting for set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.

" Allow backspacing over anything
set backspace=indent,eol,start

" netrw config
let g:netrw_banner=0
let g:netrw_keepdir=0
let g:netrw_liststyle=0
let g:netrw_browse_split=0
let g:netrw_preview=1

" UTF-8 everywhere
set encoding=utf-8

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

Plug 'lifepillar/vim-solarized8'

Plug 'rust-lang/rust.vim', { 'for': ['rust'] }
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }

Plug 'preservim/tagbar'

Plug 'lotabout/skim'
Plug 'lotabout/skim.vim'

Plug 'tpope/vim-speeddating'
Plug 'vim-scripts/utl.vim'
Plug 'jceb/vim-orgmode'

" For Lisps
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'jpalardy/vim-slime', { 'branch': 'main' }

"For Clojure
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-clojure-highlight'
Plug 'tpope/vim-fireplace'

Plug 'wlangstroth/vim-racket'

Plug 'vmchale/dhall-vim'

Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-unimpaired'

call plug#end()

" Plugin config that must be specified after Plugged has finished

" Solarized colorscheme
" Includes config to auto switch background with and without tmux
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

function! s:checkCurrentTermBackgroundColor()
  if exists("$TMUX")
    let &t_RB="\ePtmux;\e\e]11;?\007\e\\"
    call echoraw(&t_RB)
    let &t_RB="\e]11;?\007"
  endif
  call echoraw(&t_RB)
endfunction

function! s:handleBackgroundCheckTimer(timer)
  call s:checkCurrentTermBackgroundColor()
endfunction

call timer_start(500, 's:handleBackgroundCheckTimer', {'repeat': -1})
let g:solarized_italics=0
silent! colorscheme solarized8

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

" Tags config for rust
autocmd BufRead *.rs :setlocal tags=./rusty-tags.vi;/,$RUST_SRC_PATH/rusty-tags.vi

" For OrgMode files
autocmd FileType org setlocal foldlevel=99

" Fix indentation for yaml files when commenting out a section
autocmd FileType yaml setlocal expandtab indentkeys-=0#

" Slime config
" Open Slime with C-c, v, start repl program in terminal
let g:slime_target = "vimterminal"
let g:slime_vimterminal_cmd = "/usr/bin/env zsh"
let g:slime_vimterminal_config = {"vertical": 1, "term_finish": "close"}

" Shortcut to toggle wrap
nnoremap <silent> <localleader>w :set wrap!<cr>

" Shortcut to toggle paste
nnoremap <silent> <localleader>p :set paste!<cr>
