#!/bin/zsh

. ~/.zsh/alias
. ~/.zsh/function
. ~/.zsh/autotime
PURE_PROMPT_SYMBOL="➜" . ~/.zsh/pure.zsh
. ~/.zsh/zsh-syntax-highlighting.zsh

# Because /etc/profile may have overriden our ~/.zshenv setting
export PATH=/bin:/sbin:/usr/local/bin:/usr/bin:/usr/sbin:$HOME/bin:/opt/anaconda/bin

