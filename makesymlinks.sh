#!/usr/bin/env sh

dotfiles=${HOME}/dotfiles
files=".tmux.conf .gitconfig .vimrc .vim .zsh .zprofile .zshenv .zshrc .zfunc .emacs.d"

echo "Symlinking dotfiles from ${dotfiles} to ${HOME}..."
mkdir -p ${dotfiles}

for file in $files; do
    if [ ! -e ${HOME}/${file} ]; then
        ln -s ${dotfiles}/${file} ~/${file}
    fi
done

mkdir -p ${HOME}/.config/htop
if [ ! -e ${HOME}/.config/htop/htoprc ]; then
  ln -s ${dotfiles}/htoprc ${HOME}/.config/htop/htoprc
fi

ln -s ${dotfiles}/broot ${HOME}/.config/broot

ln -s ${dotfiles}/nvim ${HOME}/.config/nvim

ln -s ${dotfiles}/ghostty ${HOME}/.config/ghostty

# Pi mixes configuration with credentials and runtime data. Link only public config.
pi_files="AGENTS.md settings.json keybindings.json pi-vimmode.config.js themes extensions/subagent/config.json extensions/firecrawl.json extensions/primary-model.js"
for file in $pi_files; do
    source="${dotfiles}/pi/agent/${file}"
    target="${HOME}/.pi/agent/${file}"
    if [ -e "$target" ] || [ -L "$target" ]; then
        echo "Skipping existing Pi config: ${target}"
    elif [ ! -e "$source" ]; then
        echo "Skipping missing Pi config source: ${source}" >&2
    else
        mkdir -p "$(dirname "$target")"
        ln -s "$source" "$target"
    fi
done

echo "Done"
