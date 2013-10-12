zstyle ':completion:*' menu select
zstyle '*' single-ignored show
zstyle ':completion:*' list-colors ''
zstyle :compinstall filename '/home/vagrant/.zshrc'

autoload -Uz compinit
compinit

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob nomatch promptsubst
unsetopt notify

bindkey -v
bindkey '^R' history-incremental-search-backward
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

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

alias ls="ls -G"
alias l='ls -alh'

function curlj {
    curl -s "$@" | python -mjson.tool | less -X -F;
}

if [ -e ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi
