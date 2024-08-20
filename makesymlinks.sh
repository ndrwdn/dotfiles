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

echo "Done"
