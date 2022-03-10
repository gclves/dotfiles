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
export GOPATH=$HOME/go
export PATH="$HOME/bin:/usr/local/bin:$GOPATH/bin:/usr/local/opt/postgresql@13/bin:$PATH"
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
eval "$(pyenv init -)"
eval "$(rbenv init - zsh)"

NEURON_HOME=$HOME/src/neuron-site

z() { neuron -d $NEURON_HOME $@ }
ze() { $EDITOR `z new` }
zo() { $EDITOR `z search` }

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
t() {
    bundle exec rspec `fzf --multi -q spec/`
}

rh() {
    . $HOME/.zshrc
}

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

