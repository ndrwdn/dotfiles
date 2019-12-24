#!/usr/bin/env bash

dotfiles=~/dotfiles
files=".tmux .tmux.conf .gitconfig .vimrc .vim .zsh .zshrc .zfunc .emacs.d"

echo -n "Symlinking dotfiles from ${dotfiles} to ~..."
mkdir -p ${dotfiles}

for file in $files; do
    if [[ ! -L ~/${file} ]]; then
        ln -s ${dotfiles}/${file} ~/${file}
    fi
done

mkdir -p ${HOME}/.lein
ln -s ${dotfiles}/profiles.clj ${lein_profiles}

mkdir -p ${HOME}/.config/htop
ln -s ${dotfiles}/htoprc ${HOME}/.config/htop/htoprc

echo "done."
