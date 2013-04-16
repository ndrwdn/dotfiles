#!/bin/bash

dotfiles=~/dotfiles
backup=~/dotfiles/backup
files=".tmux.conf .htoprc .vimrc .vim"

echo -n "Backing up any existing dotfiles in ~ to ${backup}..."
mkdir -p ${backup}

for file in $files; do
    if [[ -f ~/${file} && !(-L ~/${file}) ]]; then
        mv ~/${file} ${backup}/
    fi
done

echo "done."

echo -n "Symlinking dotfiles from ${dotfiles} to ~..."
mkdir -p ${dotfiles}

for file in $files; do
    if [[ ! -L ~/${file} ]]; then
        ln -s ${dotfiles}/${file} ~/${file}
    fi
done

echo "done."
