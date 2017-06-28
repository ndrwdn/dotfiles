zstyle ':completion:*' menu select
zstyle '*' single-ignored show
zstyle ':completion:*' list-colors ''
zstyle :compinstall filename '/home/vagrant/.zshrc'

autoload -Uz compinit
compinit

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob nomatch promptsubst histignoredups histignorespace
unsetopt notify

bindkey -v
bindkey '^R' history-incremental-search-backward
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

MODE_INDICATOR="%B%F{red}<%b%F{red}<<%f"
function zle-keymap-select zle-line-init zle-line-finish {
  VI_MODE="${${KEYMAP/vicmd/$MODE_INDICATOR}/(main|viins)/}"
  zle reset-prompt
  zle -R
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

PROMPT='
%B%F{green}%n@%m%b%F{white}:%B%F{yellow}%~%u%b%f
%F{blue}>%f '
RPROMPT='${VI_MODE}'

autoload -U colors; colors
export LSCOLORS="Gxfxcxdxbxegedabagacad"

bindkey "\eOC" forward-word
bindkey "^ " autosuggest-execute
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='bg=7'

if [[ $(uname -s) == "Darwin" ]]; then
  alias ls="ls -G"
else
  alias ls="ls --color=always"
fi

alias l='ls -alh'

alias gpr="git pull --rebase"
alias gst="st"

function curlj {
    curl -s "$@" | python -mjson.tool | less -X -F;
}

# make dir and cd into it
mcd () { mkdir -p $1 && cd $1 }

export PAGER="~/.vim/plugged/vimpager/vimpager"

if [ -e ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi
