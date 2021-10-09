# This file is sourced for INTERACTIVE shells

. /etc/ksh.kshrc

export GOPATH=$HOME/go
PLAN9=/usr/local/plan9
export EDITOR=nvim
export VISUAL=nvim
export MANPATH=/usr/share/man:/usr/X11R6/man:/usr/local/man:/usr/local/share/man/:/usr/local/lib/tcl/tcl8.5/man:/usr/local/lib/tcl/tk8.5/man
export PATH=$HOME/bin:$HOME/.local/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games:$PLAN9/bin:$GOPATH/bin
export HOME TERM

set -o emacs
alias vi=nvim
alias x=startx
alias python=python3
alias venv='python -m venv'
alias ll='ls -lh'
alias t='task'
alias pass='gopass'

alias n="$EDITOR $HOME/notes"

alias weather='curl http://wttr.in/London'
alias ip='curl http://ifconfig.co/json'

eval `gopass completion openbsdksh`

PS1="\w % "
fortune
# randverse

