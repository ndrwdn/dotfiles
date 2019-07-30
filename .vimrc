" Plugins and plugin-specific config
"""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

Plug 'altercation/vim-colors-solarized'

call plug#end()

" Plugin config that must be specified after Plugged has finished
set background=dark
colorscheme solarized


" Non-plugin-base settings below here
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

" Indentation settings
set expandtab
set shiftround
set smartindent
set shiftwidth=2
set softtabstop=2
set tabstop=2

" Fix indentation for yaml files when commenting out a section
autocmd BufNewFile,BufRead *.yaml,*.yml setlocal indentkeys-=0#
