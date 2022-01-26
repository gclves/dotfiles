#!/bin/zsh

export PROJECT_ROOT="$HOME/src"
export EDITOR=nvim
export VISUAL="$(which subl) -w"

set -o emacs
autoload -Uz compinit
compinit

ZSH_D=$HOME/.zsh.d
. $ZSH_D/bootstrap

export REVIEW_BASE=master
export PATH="$HOME/bin:/usr/local/bin:/usr/local/opt/postgresql@13/bin:$PATH"
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'

