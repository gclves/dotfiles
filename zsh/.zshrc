#!/bin/zsh

export GPG_TTY="$(tty)"
export PROJECT_ROOT="$HOME/src"
export EDITOR="emacsclient -c -a ''"
export VISUAL=$EDITOR
export HUSKY=0

set -o emacs

autoload -Uz compinit
compinit

rh() {
    . $HOME/.zshrc
}

export LDFLAGS="-L/usr/local/opt/qt@5/lib"
export CPPFLAGS="-I/usr/local/opt/qt@5/include"

ZSH_D=$HOME/.zsh.d
. $ZSH_D/bootstrap
