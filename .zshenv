#
# Dircolors...
#eval `dircolors -b ~/.dircolors`

# Kill flow control
if tty -s ; then
  stty -ixon
  stty -ixoff
fi

# Exports
export PATH=/bin:/sbin:/usr/local/bin:/usr/bin:/usr/sbin:$HOME/bin/opt/anaconda/bin
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LOCALE=en_US.UTF-8
export BROWSER='chromium'
export OOO_FORCE_DESKTOP='gnome'
export EDITOR='emacsclient -a "" -t'
export VISUAL='emacsclient -a ""'
export HISTFILE='~/.zsh/history'
export SAVEHIST=1000
export HISTSIZE=1000
export IGNOREEOF=3
export EMAIL="guilhermeaugustosg@gmail.com"
export NAME="Guilherme Gonçalves"
export SMTPSERVER="smtp.gmail.com"

# History
setopt SHARE_HISTORY
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS

if [ "$TERM" = "linux" ]; then
  echo -en "\e]P0000000" #black
  echo -en "\e]P8505354" #darkgrey
  echo -en "\e]P1f92672" #darkred
  echo -en "\e]P9ff5995" #red
  echo -en "\e]P282b414" #darkgreen
  echo -en "\e]PAb6e354" #green
  echo -en "\e]P3fd971f" #brown
  echo -en "\e]PBfeed6c" #yellow
  echo -en "\e]P456c2d6" #darkblue
  echo -en "\e]PC8cedff" #blue
  echo -en "\e]P58c54fe" #darkmagenta
  echo -en "\e]PD9e6ffe" #magenta
  echo -en "\e]P6465457" #darkcyan
  echo -en "\e]PE899ca1" #cyan
  echo -en "\e]P7ccccc6" #lightgrey
  echo -en "\e]PFf8f8f2" #white
  clear # back to default input colours
fi

bindkey -e
bindkey "\e[1~" beginning-of-line
bindkey "\e[7~" beginning-of-line
bindkey "\e[8~" end-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[3~" delete-char

bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history

bindkey '^R' history-incremental-search-backward

fpath=(~/.zfunctions ~/.zsh/completion $fpath)
autoload -U compinit
compinit

#
# setup ssh-agent
#


# set environment variables if user's agent already exists
[ -z "$SSH_AUTH_SOCK" ] && SSH_AUTH_SOCK=$(ls -l /tmp/ssh-*/agent.* 2> /dev/null | grep $(whoami) | awk '{print $9}')
[ -z "$SSH_AGENT_PID" -a -z `echo $SSH_AUTH_SOCK | cut -d. -f2` ] && SSH_AGENT_PID=$((`echo $SSH_AUTH_SOCK | cut -d. -f2` + 1))
[ -n "$SSH_AUTH_SOCK" ] && export SSH_AUTH_SOCK
[ -n "$SSH_AGENT_PID" ] && export SSH_AGENT_PID

# start agent if necessary
if [ -z $SSH_AGENT_PID ] && [ -z $SSH_TTY ]; then  # if no agent & not in ssh
  eval `ssh-agent -s` > /dev/null
fi

# setup addition of keys when needed
if [ -z "$SSH_TTY" ] ; then                     # if not using ssh
  ssh-add -l > /dev/null                        # check for keys
  if [ $? -ne 0 ] ; then
    alias ssh='ssh-add -l > /dev/null || ssh-add && unalias ssh ; ssh'
    if [ -f "/usr/lib/ssh/x11-ssh-askpass" ] ; then
      SSH_ASKPASS="/usr/lib/ssh/x11-ssh-askpass" ; export SSH_ASKPASS
    fi
  fi
fi
