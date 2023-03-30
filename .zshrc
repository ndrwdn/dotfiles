export LANG="en_US.UTF-8"

zstyle ':completion:*' menu select
zstyle '*' single-ignored show
zstyle ':completion:*' list-colors ''
zstyle :compinstall filename '/home/vagrant/.zshrc'

fpath+=~/.zfunc

autoload -Uz compinit
compinit

HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory autocd extendedglob interactivecomments nomatch promptsubst histignoredups histignorespace
unsetopt notify
autoload zmv

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

autoload -U colors; colors

bindkey "\eOC" forward-word
bindkey "^ " autosuggest-execute
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='standout'

export LS_COLORS='no=00:fi=00:di=36:ow=34;47:ln=35:pi=30;44:so=35;44:do=35;44:bd=33;44:cd=37;44:or=05;37;41:mi=05;37;41:ex=01;31:*.cmd=01;31:*.exe=01;31:*.com=01;31:*.bat=01;31:*.reg=01;31:*.app=01;31:*.txt=32:*.org=32:*.md=32:*.mkd=32:*.h=32:*.hpp=32:*.c=32:*.C=32:*.cc=32:*.cpp=32:*.cxx=32:*.objc=32:*.cl=32:*.sh=32:*.bash=32:*.csh=32:*.zsh=32:*.el=32:*.vim=32:*.java=32:*.pl=32:*.pm=32:*.py=32:*.rb=32:*.hs=32:*.php=32:*.htm=32:*.html=32:*.shtml=32:*.erb=32:*.haml=32:*.xml=32:*.rdf=32:*.css=32:*.sass=32:*.scss=32:*.less=32:*.js=32:*.coffee=32:*.man=32:*.0=32:*.1=32:*.2=32:*.3=32:*.4=32:*.5=32:*.6=32:*.7=32:*.8=32:*.9=32:*.l=32:*.n=32:*.p=32:*.pod=32:*.tex=32:*.go=32:*.sql=32:*.csv=32:*.bmp=33:*.cgm=33:*.dl=33:*.dvi=33:*.emf=33:*.eps=33:*.gif=33:*.jpeg=33:*.jpg=33:*.JPG=33:*.mng=33:*.pbm=33:*.pcx=33:*.pdf=33:*.pgm=33:*.png=33:*.PNG=33:*.ppm=33:*.pps=33:*.ppsx=33:*.ps=33:*.svg=33:*.svgz=33:*.tga=33:*.tif=33:*.tiff=33:*.xbm=33:*.xcf=33:*.xpm=33:*.xwd=33:*.xwd=33:*.yuv=33:*.aac=33:*.au=33:*.flac=33:*.m4a=33:*.mid=33:*.midi=33:*.mka=33:*.mp3=33:*.mpa=33:*.mpeg=33:*.mpg=33:*.ogg=33:*.opus=33:*.ra=33:*.wav=33:*.anx=33:*.asf=33:*.avi=33:*.axv=33:*.flc=33:*.fli=33:*.flv=33:*.gl=33:*.m2v=33:*.m4v=33:*.mkv=33:*.mov=33:*.MOV=33:*.mp4=33:*.mp4v=33:*.mpeg=33:*.mpg=33:*.nuv=33:*.ogm=33:*.ogv=33:*.ogx=33:*.qt=33:*.rm=33:*.rmvb=33:*.swf=33:*.vob=33:*.webm=33:*.wmv=33:*.doc=31:*.docx=31:*.rtf=31:*.odt=31:*.dot=31:*.dotx=31:*.ott=31:*.xls=31:*.xlsx=31:*.ods=31:*.ots=31:*.ppt=31:*.pptx=31:*.odp=31:*.otp=31:*.fla=31:*.psd=31:*.7z=1;35:*.apk=1;35:*.arj=1;35:*.bin=1;35:*.bz=1;35:*.bz2=1;35:*.cab=1;35:*.deb=1;35:*.dmg=1;35:*.gem=1;35:*.gz=1;35:*.iso=1;35:*.jar=1;35:*.msi=1;35:*.rar=1;35:*.rpm=1;35:*.tar=1;35:*.tbz=1;35:*.tbz2=1;35:*.tgz=1;35:*.tx=1;35:*.war=1;35:*.xpi=1;35:*.xz=1;35:*.z=1;35:*.Z=1;35:*.zip=1;35:*.ANSI-30-black=30:*.ANSI-01;30-brblack=01;30:*.ANSI-31-red=31:*.ANSI-01;31-brred=01;31:*.ANSI-32-green=32:*.ANSI-01;32-brgreen=01;32:*.ANSI-33-yellow=33:*.ANSI-01;33-bryellow=01;33:*.ANSI-34-blue=34:*.ANSI-01;34-brblue=01;34:*.ANSI-35-magenta=35:*.ANSI-01;35-brmagenta=01;35:*.ANSI-36-cyan=36:*.ANSI-01;36-brcyan=01;36:*.ANSI-37-white=37:*.ANSI-01;37-brwhite=01;37:*.log=01;34:*~=01;34:*#=01;34:*.bak=01;36:*.BAK=01;36:*.old=01;36:*.OLD=01;36:*.org_archive=01;36:*.off=01;36:*.OFF=01;36:*.dist=01;36:*.DIST=01;36:*.orig=01;36:*.ORIG=01;36:*.swp=01;36:*.swo=01;36:*,v=01;36:*.gpg=34:*.gpg=34:*.pgp=34:*.asc=34:*.3des=34:*.aes=34:*.enc=34:*.sqlite=34:';
alias ls="ls --color=auto"


alias l='ls -alh'
alias clean='sed -E -e "s/$(printf "\x1b")\[[0-9;]*[a-zA-Z]//g"'
alias vv='vim -R --not-a-term -'

alias gpr="git pull --rebase"
alias gst="st"
alias gob="git checkout \$(git branch --format '%(refname:short)' --sort '-committerdate' | sk)"
alias gorb='git checkout $(git reflog | rg " checkout: moving from" | awk "{print \$NF}" | rg -f<(git branch --format="%(refname:short)") | rg -v ${$(git branch --show-current):-\#\#not-a-branch\#\#} | cat -n | sort -uk2 | sort -nk1 | head | awk "{print \$NF}" | sk)'

alias tsfmt="ts '%FT%.T%z'"

alias make_backup="rsync -ah --info=progress2 --no-whole-file --inplace"
alias last-duration="fc -liD -1"

function update-dotfiles() {
  pushd ~/dotfiles
  git pull --rebase
  popd
}

function grt() {
  cd $(git rev-parse --show-toplevel)
}

function command_exists() {
  command -v "${1}" >/dev/null 2>&1
}

export EDITOR=vim

if [[ "$(uname -s)" == "Darwin" ]] then
  typeset -r GREP_PATH=$(find /usr/local/Cellar/grep -maxdepth 1 -type d | sort -V -r | head -n1)
  export PATH="${HOME}/Applications:${HOME}/programs/bin:${HOME}/.cabal/bin:/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/gnu-sed/libexec/gnubin:/usr/local/opt/findutils/libexec/gnubin:/usr/local/opt/gawk/libexec/gnubin:/usr/local/opt/util-linux/bin:/usr/local/opt/util-linux/sbin:/usr/local/opt/gnu-tar/libexec/gnubin:${GREP_PATH}/libexec/gnubin:/usr/local/opt/curl/bin:/usr/local/opt/gettext/bin:/usr/local/sbin:${PATH}"

  alias vlc="/Applications/VLC.app/Contents/MacOS/VLC"
  alias emacs="emacs -nw"
  alias ffp="/Applications/Firefox.app/Contents/MacOS/firefox --private-window"

  function show-brew-packages() {
    sed -E -n '/(Renamed|Deleted) (Formulae|Casks)/q;p' |
      rg -v '^==>|Already up-to-date.|No changes to formulae.|Updated.*tap|Updated Homebrew|You have.*installed|brew upgrade|brew outdated' |
      tr '\n' ' ' |
      sed -e 's/✔//g' -e 's/ \{1,\}/ /g' |
      ifne xargs brew info --json=v2 |
      jq -r '[(.formulae[] | {name, desc, homepage}), (.casks[] | {name: .token, desc: (.desc // " "), homepage})] | .[] | .name + "\u0001" + .desc + "\u0001" + .homepage' |
      sort |
      column -ts$(printf '\x01') |
      sed -e '1i\\' -e '$a\\'
  }
  
  function update() {
    echo "Updating brew package list..."
    echo
    brew update 2>&1 | tee /dev/tty | show-brew-packages
    echo
    sleep 0.2s
    echo "Finding outdated packages..."
    outdated=$(brew outdated)
    if [[ $(echo ${outdated} | sed '/^$/d' | wc -l) -gt 0 ]]; then
      echo -e "\n${outdated}"
      read 'do_update?Update packages (y/n)? '
      if [[ "${do_update}" == "y" ]]; then
        echo "Updating packages..."
        brew upgrade
        sleep 0.2s
        echo "Cleaning up..."
        brew cleanup
      else
        echo "Not updating packages"
      fi
    else
      echo "No outdated packages found"
    fi
    echo "Done"
  }

  export SKIM_CTRL_R_OPTS="--with-nth=2.. --inline-info --preview='echo {} | sed -E -e \"s/^\s*[0-9]+\s\s//\"' --preview-window=down:5:wrap:hidden --regex --bind 'ctrl-v:toggle-preview' --no-sort"
  for f in /usr/local/Cellar/sk/*/share/zsh/site-functions/*.zsh; do
    source "${f}"
  done

  function osx-check-updates() {
    softwareupdate --all --list
  }

  function osx-install-update() {
    typeset -r update_name="${1}"
    sudo softwareupdate --install --restart "${update_name}"
  }

  function update-all() {
    update && update-dotfiles && update-vim && update-emacs && osx-check-updates
    date -Isec
  }

  java-version-manager() {
    [[ ! $# -eq 2 ]] && echo -e "Wrong number of arguments.\nUsage: ${0} <major_version> <true|false>" && return 1

    typeset -r major_version="${1}"
    typeset -r make_default="${2}"

    mkdir -p ~/.cache/java
    (
    cd ~/.cache/java
    typeset -ra release_fields=("${(f)$(curl \
      -s \
      "https://api.adoptium.net/v3/assets/feature_releases/${major_version}/ga?os=mac&architecture=x64&image_type=jdk&sort_method=DATE&sort_order=DESC&page_size=1" |
        jq -r '.[] | .release_name + "\n" + (.binaries[].package | .name + "\n" + .link + "\n" + .checksum)'
     )}")
    typeset -r version="${release_fields[1]}"
    typeset -r tarball="${release_fields[2]}"
    typeset -r url="${release_fields[3]}"
    typeset -r sha="${release_fields[4]}"

    if [[ ! -d "${version}" ]]; then
      read "do_install?Install JDK ${version} (y/n)? "
      if [[ "${do_install}" == "y" ]]; then
        curl -L "${url}" -o "${tarball}"

        typeset -r actual_sha="$(sha256sum ${tarball} | awk '{print $1}')"
        if [[ "${sha}" != "${actual_sha}" ]]; then
          echo "Error: shasums don't match"
          exit 1
        fi

        tar xf "${tarball}"
        rm -rf "${tarball}"

        if [[ "${make_default}" == "true" ]]; then
          rm default
          ln -s "${version}" default

          if [[ ! -e "/Library/Java/JavaVirtualMachines/default" ]]; then
            sudo ln -s ~/.cache/java/default /Library/Java/JavaVirtualMachines/default
          fi
        fi
      fi
    fi
    )
  }

  clipboard-to-title-case() {
    pbpaste | tr '[:upper:]' '[:lower:]' | sed -E -e 's/\b(.)/\u\1/g' | pbcopy
  }

  export HOMEBREW_NO_ANALYTICS=1
  export HOMEBREW_NO_AUTO_UPDATE=1
fi


if command_exists broot; then
  function br() {
      f=$(mktemp)
      (
          set +e
          broot --outcmd "$f" "$@"
          code=$?
          if [ "$code" != 0 ]; then
              rm -f "$f"
              exit "$code"
          fi
      )
      code=$?
      if [ "$code" != 0 ]; then
          return "$code"
      fi
      d=$(<"$f")
      rm -f "$f"
      eval "$d"
  }
fi

if command_exists bat; then
  export BAT_THEME="Solarized (dark)"
  export MANPAGER="sh -c 'col -bx | bat -l man -p'"
fi

if command_exists git; then
  alias diff="git diff --stat=120 --patch --no-index"
fi

if command_exists entr; then
  rdev() {
    (
      interrupted=0
      trap 'interrupted=1' INT
      while [[ ${interrupted} -eq 0 ]]; do
        fd '.*\.rs' | entr -d -n -s 'cargo fmt --all -v; rusty-tags vi'
      done
    )
  }
fi

if command_exists vim; then
  update-vim() {
    vim +PlugUpdate +qall
  }
fi

if command_exists emacs; then
  update-emacs() {
    emacs --batch --load ~/.emacs.d/init.el --eval '(auto-package-update-now)'
  }
fi

if command_exists gh; then
  prs-review() {
    prs-query 'is:open' 'user-review-requested:@me'
  }

  prs-mine() {
    prs-query 'is:open' 'author:@me'
  }

  prs-query() {
    [[ $# -lt 1 ]] && echo -e "Wrong number of arguments.\nUsage: ${0} <query-term-1> ... <query-term-n>" && return 1
    typeset -r pr_query_terms=(${@})
    typeset -ra pr_query_results=("${(@f)$(gh search prs \
      ${pr_query_terms[@]} \
      --json author,number,title,updatedAt,url |
      jq -r '.[] | .updatedAt + "\u0001" + (.number | tostring) + "\u0001" + .title + "\u0001" + .author.login + "\u0001" + .url' |
      column -ts$(printf '\x01') |
      sort -r)}")

    if [[ "${#pr_query_results[@]}" -gt 0  && "${pr_query_results[1]}" != '' ]]; then
      typeset -r selected_pr_url="$(echo ${(F)pr_query_results[@]} | sk | awk '{print $NF}')"
      if [[ "${selected_pr_url}" != '' ]]; then
        open "${selected_pr_url}"
      fi
    else
      echo "no results"
    fi
  }
fi
