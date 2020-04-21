# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize

# Disable completion when the input buffer is empty.  i.e. Hitting tab
# and waiting a long time for bash to expand all of $PATH.
shopt -s no_empty_cmd_completion

# Enable history appending instead of overwriting when exiting.  #139609
shopt -s histappend

# Save each command to the history file as it's executed.  #517342
# This does mean sessions get interleaved when reading later on, but this
# way the history is always up to date.  History is not synced across live
# sessions though; that is what `history -n` does.
# Disabled by default due to concerns related to system recovery when $HOME
# is under duress, or lives somewhere flaky (like NFS).  Constantly syncing
# the history will halt the shell prompt until it's finished.
PROMPT_COMMAND='history -a'

# Change the window title of X terminals
case ${TERM} in
    [aEkx]term*|rxvt*|gnome*|konsole*|interix)
        PS1='\[\033]0;\u@\h:\w\007\]'
        ;;
    screen*)
        PS1='\[\033k\u@\h:\w\033\\\]'
        ;;
esac

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.
use_color=false
if type -P dircolors >/dev/null ; then
    # Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
    LS_COLORS=
    if [[ -f ~/.dir_colors ]] ; then
        # If you have a custom file, chances are high that it's not the default.
        used_default_dircolors="no"
        eval "$(dircolors -b ~/.dir_colors)"
    elif [[ -f /etc/DIR_COLORS ]] ; then
        # People might have customized the system database.
        used_default_dircolors="maybe"
        eval "$(dircolors -b /etc/DIR_COLORS)"
    else
        used_default_dircolors="yes"
        eval "$(dircolors -b)"
    fi
    if [[ -n ${LS_COLORS:+set} ]] ; then
        use_color=true

        # The majority of systems out there do not customize these files, so we
        # want to avoid always exporting the large $LS_COLORS variable.  This
        # keeps the active env smaller, and it means we don't have to deal with
        # running new/old (incompatible) versions of `ls` compared to when we
        # last sourced this file.
        case ${used_default_dircolors} in
            no) ;;
            yes) unset LS_COLORS ;;
            *)
                ls_colors=$(eval "$(dircolors -b)"; echo "${LS_COLORS}")
                if [[ ${ls_colors} == "${LS_COLORS}" ]] ; then
                    unset LS_COLORS
                fi
                ;;
        esac
    fi
    unset used_default_dircolors
else
    # Some systems (e.g. BSD & embedded) don't typically come with
    # dircolors so we need to hardcode some terminals in here.
    case ${TERM} in
        [aEkx]term*|rxvt*|gnome*|konsole*|screen|cons25|*color) use_color=true;;
    esac
fi

if ${use_color} ; then
    export CLICOLOR=1
    alias ls='ls --color=auto'
    alias grep='grep --colour=auto'
    alias egrep='egrep --colour=auto'
    alias fgrep='fgrep --colour=auto'
    . ~/.bash_prompt
fi

# Try to keep environment pollution down, EPA loves us.
unset use_color sh

export VISUAL='emacsclient -a ""'
export EDITOR="$VISUAL -t"
export VAGRANT_DEFAULT_PROVIDER=virtualbox
export PLAN9=/usr/local/plan9
export PATH=$PATH:$HOME/.local/bin:$HOME/bin:node_modules/.bin:$PLAN9/bin:$HOME/Library/Python/2.7/bin

# Aliases
alias e=$VISUAL
alias n='terminal_velocity'     # quick notes

function lt() { ls -ltrsa "$@" | tail; }
function psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto; }
function fname() { find . -iname "*$@*"; }
function remove_lines_from() { grep -F -x -v -f $2 $1; }
alias pp="ps axuf | pager"
alias sum="xargs | tr ' ' '+' | bc" ## Usage: echo 1 2 3 | sum
alias lz='ls -ltsraZ'
function mcd() { mkdir $1 && cd $1; }
function sanitize() {
    find $1 -type f -exec chmod 644 {} +
    find $1 -type d -exec chmod 755 {} +
}
alias vi=nvim

# Important security announcements
dig +short txt istheinternetonfire.com
