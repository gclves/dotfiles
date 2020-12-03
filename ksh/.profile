# $OpenBSD: dot.profile,v 1.7 2020/01/24 02:09:51 okan Exp $
#
# sh/ksh initialization

export PLAN9=/opt/plan9
export EDITOR=nvim
export VISUAL=nvim
export GOPATH=$HOME/go
PATH=$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games:$PLAN9/bin:$GOPATH/bin
export PATH HOME TERM
export ENV=$HOME/.kshrc

export MANPATH=/usr/share/man:/usr/X11R6/man:/usr/local/man:/usr/local/share/man/
