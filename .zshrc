# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="juanghurtado"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(autojump vi-mode)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
unsetopt hist_verify
unsetopt share_history
zle -A .self-insert self-insert

bindkey '^R' history-incremental-search-backward
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

alias fm="vim -c 'VE .'"
alias rscp="rsync -aPh --no-whole-file --inplace"
alias rsmv="rscp --remove-source-files"
alias mc="EDITOR=/usr/bin/vim && mc -b"
alias hexdump="hexdump -C"
alias g="gvim --remote-silent"
alias vlc="/Applications/VLC.app/Contents/MacOS/VLC"
#alias ack='ack --color --group --pager="less -R"'
alias runSimpleHttpServer='python -m SimpleHTTPServer 8000'

export PATH=~/Applications:~/programs/bin:/usr/local/bin:/Library/Java/JavaVirtualMachines/jdk1.8.0.jdk.lambdas/Contents/Home/bin:"$PATH"
#export MANPATH=~/programs/share/man:"$MANPATH"

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

function curlj {
    curl -s "$@" | python -mjson.tool | less -X -F;
}

# get the name of the branch we are on
#function git_prompt_info() {
#  ref=$(git symbolic-ref HEAD 2> /dev/null) || \
#  ref=$(git rev-parse --short HEAD 2> /dev/null) || return
#  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$ZSH_THEME_GIT_PROMPT_SUFFIX"
#}

PROMPT='
%{$GREEN_BOLD%}%n@%m%{$WHITE%}:%{$YELLOW%}%~%u%{$RESET_COLOR%}
%{$BLUE%}>%{$RESET_COLOR%} '
RPROMPT='$(vi_mode_prompt_info) %{$GREEN_BOLD%}%{$RESET_COLOR%}'

if [ -e ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi
