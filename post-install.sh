#!/bin/sh

install_dotfiles() {
   git clone https://git.sr.ht/~gg/dotfiles $HOME/dotfiles
   cd $HOME/dotfiles
   stow *
}

mkdir -p $HOME/src/ $HOME/bin

# install_dotfiles

# TODO: set up cloud storage
# TODO: install aerc
# TODO: set up SSH key
# TODO: run offlineimap on crontab
mkdir -p "~/sync/Archive/$(date +%Y)" && ln -s ~/sync/Archive/$(date +%Y) ~/wrk

