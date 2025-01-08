#!/bin/zsh

export GPG_TTY="$(tty)"
export PROJECT_ROOT="$HOME/src"
export EDITOR=hx
export VISUAL="$(which subl) -w"

set -o vi
bindkey -M viins 'jk' vi-cmd-mode

autoload -Uz compinit
compinit

export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
rh() {
    . $HOME/.zshrc
}

export LDFLAGS="-L/usr/local/opt/qt@5/lib"
export CPPFLAGS="-I/usr/local/opt/qt@5/include"

ZSH_D=$HOME/.zsh.d
. $ZSH_D/bootstrap
