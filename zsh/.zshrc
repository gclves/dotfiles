#!/bin/zsh

export PROJECT_ROOT="$HOME/src"
export EDITOR=nvim

set -o emacs
autoload -Uz compinit
compinit

ZSH_D=$HOME/.zsh.d
. $ZSH_D/bootstrap

export REVIEW_BASE=master
export GOPATH=$HOME/go
export PLAN9=/opt/plan9
export PATH="$HOME/bin:/usr/local/bin:$GOPATH/bin:$PLAN9/bin:/usr/local/opt/postgresql@13/bin:$PATH"
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
# eval "$(pyenv init --path)"
# eval "$(pyenv init -)"
# eval "$(rbenv init - zsh)"


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
rh() {
    . $HOME/.zshrc
}

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

export PATH="/usr/local/opt/openjdk/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/qt@5/lib"
export CPPFLAGS="-I/usr/local/opt/qt@5/include"

export VISUAL="$(which subl) -w"
