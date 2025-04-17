zstyle ':completion:*' menu select
zstyle '*' single-ignored show
zstyle ':completion:*' list-colors ''

autoload -Uz compinit
compinit

HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory autocd extendedglob interactivecomments nomatch promptsubst histignoredups histignorespace
unsetopt notify

bindkey -v
bindkey '^R' history-incremental-pattern-search-backward
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

PROMPT='; '
RPROMPT='${VI_MODE}'

bindkey "\eOC" forward-word
bindkey "^ " autosuggest-execute
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='standout'

if command_exists fzf; then
  export FZF_CTRL_R_OPTS="--wrap"
  source <(fzf --zsh)
fi

if [[ -f "${HOME}/.zshrc.local" ]]; then
  source "${HOME}/.zshrc.local"
fi
