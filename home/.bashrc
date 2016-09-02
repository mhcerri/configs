#
# ~/.bashrc
#

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=20000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Source any distro specific base shell
# TODO: check why /etc/bash.bashrc is printing a new line...
[ -f /etc/bash.bashrc ] && . /etc/bash.bashrc &> /dev/null
[ -f /etc/bashrc ] && . /etc/bashrc &> /dev/null

#
# Set prompt
#
case "$TERMINAL" in
    terminology)
        export  TERM=xterm-256color
        ;;
esac

# Uncomment for color prompt when supported
color_prompt_when_supported=yes

# Check for color support
if [ -n "$color_prompt_when_supported" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >& /dev/null; then
        color_prompt=yes
    fi
fi

# Define prompt fragments
JOBS_PS1='$(
    N="$(jobs 2> /dev/null | wc -l)"
    if [[ -n "$N" ]] && [[ "$N" != 0 ]]; then
        if [[ "$N" == 1 ]]; then
            N="${N} job"
        else
            N="${N} jobs"
        fi
        echo "[$N] "
    fi
)'
DEBIAN_ROOT_PS1='${debian_chroot:+($debian_chroot)}'
RC_PS1='$(RC="$?"; [[ "$RC" -ne 0 ]] && echo -n "[rc=$RC] ")'
LOGIN_PS1='\u@\h'
WINDOW_PS1="$([[ -n "$WINDOW" ]] && echo -n "[w$WINDOW]")"
PWD_PS1='\w'
GIT_PS1='$(__git_ps1 " (%s)" 2> /dev/null)'

# Add color to fragments
if [ "$color_prompt" = yes ]; then
    # No color for DEBIAN_ROOT_PS1
    [ -n "$RC_PS1"     ] &&     RC_PS1="\[\033[01;31m\]$RC_PS1\[\033[01;00m\]"
    [ -n "$JOBS_PS1"   ] &&   JOBS_PS1="\[\033[01;32m\]$JOBS_PS1\[\033[01;00m\]"
    [ -n "$LOGIN_PS1"  ] &&  LOGIN_PS1="\[\033[01;32m\]$LOGIN_PS1\[\033[00m\]"
    [ -n "$WINDOW_PS1" ] && WINDOW_PS1="\[\033[01;33m\]$WINDOW_PS1\[\033[00m\]"
    [ -n "$PWD_PS1"    ] &&    PWD_PS1="\[\033[01;34m\]$PWD_PS1\[\033[00m\]"
    [ -n "$GIT_PS1"    ] &&    GIT_PS1="\[\033[01;31m\]$GIT_PS1\[\033[01;00m\]"
fi

# Set prompt
export PS1="$DEBIAN_ROOT_PS1$RC_PS1$JOBS_PS1$LOGIN_PS1$WINDOW_PS1 $PWD_PS1$GIT_PS1\$ "
export SUDO_PS1="$( echo -n "$PS1" | sed -e 's/;32m/;31m/g' )"
unset color_prompt color_prompt_when_supported

#
# Aliases
#

alias ls='ls --color=auto'
alias ll='ls -l'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

#
# Path
#

# Add user local bin/ directory to path
if [[ -e "$HOME/bin" ]]; then
    export PATH="$HOME/bin:$PATH"
fi

# Add ruby gems to path
if which ruby &>/dev/null; then
    export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
fi

#
# Proxy settings
#
if [[ -z "$_PROXY" ]]; then
    _PROXY_FILE="$HOME/.proxy"
    if [[ -f "$_PROXY_FILE" ]]; then
        _PROXY=$( cat "$_PROXY_FILE" )
    fi
fi
if [[ -n "$_PROXY" ]]; then
    export http_proxy="$_PROXY"
    export https_proxy="$_PROXY"
fi

#
# Golang
#

# Add Go user dirs to path
if which go &> /dev/null; then
    export GOPATH="$HOME/go"
    export PATH="$PATH:$GOPATH/bin"
    [[ ! -e "$GOPATH" ]] && mkdir -p "$GOPATH"
fi

# Alias to set GOPATH as the current directory
alias gopath='
    export GOPATH="$PWD";
    export GO15VENDOREXPERIMENT=1;
    echo "GOPATH=\"$GOPATH\"";
    echo "GO15VENDOREXPERIMENT=\"$GO15VENDOREXPERIMENT\"";
'

#
# Python
#

export PYTHONSTARTUP="$HOME/.pyrc"
if [[ ! -e "$PYTHONSTARTUP" ]]; then
cat > "$PYTHONSTARTUP" << END
# Auto generated by ~/.bashrc
try:
    import readline
except ImportError:
    print("Module readline not available.")
else:
    import rlcompleter
    readline.parse_and_bind("tab: complete")
END
fi
if [ -e "/usr/bin/virtualenvwrapper.sh" ]; then
    . /usr/bin/virtualenvwrapper.sh
fi

#
# Local customizations
#

if [ -f ~/.bash_local ]; then
    . ~/.bash_local
fi
