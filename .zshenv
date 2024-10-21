# Load various function definitions
fpath+=~/.zfunc

if [[ -d /opt/homebrew ]]; then
  fpath+=(/opt/homebrew/share/zsh/site-functions(N))
fi

autoload zmv
autoload -U colors; colors

# Helper used below
function command_exists() {
  command -v "${1}" >/dev/null 2>&1
}

# Aliases
alias ls="ls --color=auto"
alias l='ls -alh'
alias clean='sed -E -e "s/$(printf "\x1b")\[[0-9;]*[a-zA-Z]//g"'
alias vv='nvim -R -'
alias gpr="git pull --rebase"
alias gst="st"
alias gob="git checkout \$(git branch --format '%(refname:short)' --sort '-committerdate' | sk)"
alias gorb='git checkout $(git reflog | rg " checkout: moving from" | awk "{print \$NF}" | rg -f<(git branch --format="%(refname:short)") | rg -v ${$(git branch --show-current):-\#\#not-a-branch\#\#} | cat -n | sort -uk2 | sort -nk1 | head | awk "{print \$NF}" | sk)'
alias tsfmt="ts '%FT%.T%z'"
alias make_backup="rsync -ah --info=progress2 --no-whole-file --inplace"
alias dur='fc -lD -t %FT%T%z'
alias ldur='dur -1'
alias j='jobs -l'
alias f='fg'
alias p=pretty-pwd
alias nv=nvim

if [[ "$(uname -s)" == "Darwin" ]] then
  alias emacs="emacs -nw"
  alias ffp="/Applications/Firefox.app/Contents/MacOS/firefox --private-window"
fi

if command_exists git; then
  alias diff="git diff --stat=120 --patch --no-index"
fi

# Functions
update-found-repos() {
  for repo in ${$(fd --no-ignore --hidden --type d '\.git$')/\/\.git/}; do
    echo "Updating: ${repo}"
    (
    cd "${repo}"
    if [[ $(git remote -v | rg origin) != '' ]]; then
      git fetch origin --prune
      git merge --ff-only
    else
      echo "No remote named origin found"
    fi
    )
  done
}

function update-dotfiles() {
  (
  echo "Updating dotfiles..."
  cd ~/dotfiles
  git pull --rebase
  )
}

function grt() {
  cd $(git rev-parse --show-toplevel)
}

if [[ "$(uname -s)" == "Darwin" ]] then
  function show-brew-packages() {
      ifne xargs brew info --json=v2 |
      jq -r '[(.formulae[] | {name, desc, homepage}), (.casks[] | {name: .token, desc: (.desc // " "), homepage})] | .[] | .name + "\u0001" + .desc + "\u0001" + .homepage' |
      sort |
      column -ts$(printf '\x01') |
      sed -e '1i\\' -e '$a\\'
  }

  function show-brew-packages-from-update() {
    (tee >(sed -E -n -e '/^==> (New )?Formulae/,/(==>|^\s*$)/p') >(sed -E -n -e '/^==> (New )?Casks/,/(==>|^\s*$)/p') >/dev/null) |
      rg -v '^==> ' |
      tr '\n' ' ' |
      show-brew-packages
  }

  function update() {
    echo "Updating brew package list..."
    echo
    brew update 2>&1 | tee /dev/tty | show-brew-packages-from-update
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

  function osx-check-updates() {
    echo "Checking for MacOS updates..."
    softwareupdate --all --list
  }

  function osx-install-update() {
    typeset -r update_name="${1}"
    sudo softwareupdate --install --restart "${update_name}"
  }

  function update-all() {
    echo -e "Starting at: $(date -Isec)\n"
    update && echo; update-dotfiles; echo; update-nvim && echo; osx-check-updates
    echo -e "\nEnding at: $(date -Isec)"
  }

  java-version-manager() {
    [[ ! $# -eq 2 ]] && echo -e "Wrong number of arguments.\nUsage: ${0} <major_version> <true|false>" && return 1

    typeset -rA arch_mappings=(
      i386 x64
      arm64 aarch64
    )

    typeset -r major_version="${1}"
    typeset -r make_default="${2}"

    mkdir -p ~/.cache/java
    (
    cd ~/.cache/java
    typeset -r download_arch="${arch_mappings[$(arch)]}"
    typeset -ra release_fields=("${(f)$(curl \
      -s \
      "https://api.adoptium.net/v3/assets/feature_releases/${major_version}/ga?os=mac&architecture=${download_arch}&image_type=jdk&sort_method=DATE&sort_order=DESC&page_size=1" |
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

if command_exists nvim; then
  update-nvim() {
    echo "Updating nvim plugins..."
    nvim --headless "+Lazy! update" +qa
  }
fi

if command_exists emacs; then
  update-emacs() {
    echo "Updating emacs packages..."
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

  update-repos-from-github-orgs() {
    [[ $# -lt 1 ]] && >&2 echo "Usage: $0 <org_1> [<org_2> ... <org_n>]" && return 1

    typeset -ra orgs=(${@})
    typeset -r max_repos=50

    for org in ${orgs[@]}; do
      echo "Doing org: ${org}"
      if [[ ! -d "${org}" ]]; then
        mkdir "${org}"
      fi
      (
      cd "${org}"
      gh repo list "${org}" -L${max_repos} --json name | jq -r '.[].name' | sort | while read repo; do
        echo "  Doing repo: ${repo}"
        if [[ ! -d "${repo}" ]]; then
          gh repo clone "${org}/${repo}"
        else
          (
            cd "${repo}"
            git fetch origin --prune
            git merge --ff-only
          )
        fi
      done
      )
    done
  }
fi

if command_exists glab; then
  update-repos-from-gitlab-groups() {
    [[ $# -lt 1 ]] && >&2 echo "Usage: $0 <group_1> [<group_2> ... <group_n>]" && return 1

    typeset -ra groups=(${@})
    typeset -r max_repos=50

    for group in ${groups[@]}; do
      echo "Doing group: ${group}"
      if [[ ! -d "${group}" ]]; then
        mkdir "${group}"
      fi
      (
      cd "${group}"
      glab api "groups/${group}/projects" | jq -r '.[].path' | sort | while read repo; do
        echo "  Doing repo: ${repo}"
        if [[ ! -d "${repo}" ]]; then
          glab repo clone "${group}/${repo}"
        else
          (
            cd "${repo}"
            git fetch origin --prune
            git merge --ff-only
          )
        fi
      done
      )
    done
  }
fi

if [[ -f "${HOME}/.zshenv.local" ]]; then
  source "${HOME}/.zshenv.local"
fi
