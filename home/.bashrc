#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Source any distro specific base shell
# TODO: check why /etc/bash.bashrc is printing a new line...
[ -f /etc/bash.bashrc ] && . /etc/bash.bashrc &> /dev/null
[ -f /etc/bashrc ] && . /etc/bashrc &> /dev/null

#
# Set prompt
#

# Prompt helper functions:
__jobs_ps1() {
    N="$(jobs 2> /dev/null | wc -l)"
    if [[ -z "$N" ]] || [[ "$N" == 0 ]]; then
        return
    fi
    if [[ "$N" == 1 ]]; then
        N="${N} job"
    else
        N="${N} jobs"
    fi
    echo "[$N] "
}

# Uncomment for color prompt when supported
color_prompt_when_supported=yes

# Check for color support
if [ -n "$color_prompt_when_supported" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >& /dev/null; then
        color_prompt=yes
    fi
fi

# Define prompt fragments
RC_PS1='$(RC="$?"; [[ "$RC" -ne 0 ]] && echo -n "[rc=$RC] ")'
JOBS_PS1='$(__jobs_ps1)'
LOGIN_PS1='\u@\h'
WINDOW_PS1="$([[ -n "$WINDOW" ]] && echo -n "[w$WINDOW]")"
PWD_PS1='\w'
GIT_PS1='$(__git_ps1 " (%s)" 2> /dev/null)'

# Add color to fragments
if [ "$color_prompt" = yes ]; then
    [ -n "$RC_PS1"     ] &&     RC_PS1="\[\033[01;31m\]$RC_PS1\[\033[01;00m\]"
    [ -n "$JOBS_PS1"   ] &&   JOBS_PS1="\[\033[01;32m\]$JOBS_PS1\[\033[01;00m\]"
    [ -n "$LOGIN_PS1"  ] &&  LOGIN_PS1="\[\033[01;32m\]$LOGIN_PS1\[\033[00m\]"
    [ -n "$WINDOW_PS1" ] && WINDOW_PS1="\[\033[01;33m\]$WINDOW_PS1\[\033[00m\]"
    [ -n "$PWD_PS1"    ] &&    PWD_PS1="\[\033[01;34m\]$PWD_PS1\[\033[00m\]"
    [ -n "$GIT_PS1"    ] &&    GIT_PS1="\[\033[01;31m\]$GIT_PS1\[\033[01;00m\]"
fi

# Set prompt
PS1="$RC_PS1$JOBS_PS1$LOGIN_PS1$WINDOW_PS1 $PWD_PS1$GIT_PS1\$ "
unset color_prompt color_prompt_when_supported

#
# Path
#

# Add user local bin/ directory to path
PATH="~/bin:$PATH"

# Add ruby gems to path
PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"

#
# Aliases
#
alias ls='ls --color=auto'
alias ll='ls -l'

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

#
# Proxy settings
#
#_PROXY=proxy.nimbus.local:8080
if [[ -n "$_PROXY" ]]; then
    export http_proxy="$_PROXY"
    export https_proxy="$_PROXY"
fi

