#!/bin/bash

dotfiles=~/dotfiles
backup=~/dotfiles/backup
files=".tmux.conf"

echo -n "Backing up any existing dotfiles in ~ to ${backup}..."
mkdir -p ${backup}

for file in $files; do
    if [ -f ~/${file} ]; then
        mv ~/${file} ${backup}/
    fi
done

echo "done."

echo -n "Symlinking dotfiles from ${dotfiles} to ~..."
mkdir -p ${dotfiles}

for file in $files; do
    ln -s ${dotfiles}/${file} ~/${file}
done

echo "done."
