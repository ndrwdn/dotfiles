#!/usr/bin/env bash

dotfiles=~/dotfiles
backup=~/dotfiles/backup
files=".tmux.conf .htoprc .gitconfig .vimrc .vim .zsh .zshrc .emacs.d"

echo -n "Backing up any existing dotfiles in ~ to ${backup}..."
mkdir -p ${backup}

for file in $files; do
    if [[ -f ~/${file} && !(-L ~/${file}) ]]; then
        mv ~/${file} ${backup}/
    fi
done

lein_profiles=${HOME}/.lein/profiles.clj
if [[ -f ${lein_profiles} && !(-L ${lein_profiles}) ]]; then
  mv ${lein_profiles} ${backup}/
fi

echo "done."

echo -n "Symlinking dotfiles from ${dotfiles} to ~..."
mkdir -p ${dotfiles}

for file in $files; do
    if [[ ! -L ~/${file} ]]; then
        ln -s ${dotfiles}/${file} ~/${file}
    fi
done

mkdir -p ${HOME}/.lein
ln -s ${dotfiles}/profiles.clj ${lein_profiles}

echo "done."
