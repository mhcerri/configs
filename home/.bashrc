#
# ~/.bashrc
#

# Source any distro specific base shell
# TODO: check why /etc/bash.bashrc is printing a new line...
[ -f /etc/bash.bashrc ] && . /etc/bash.bashrc &> /dev/null
[ -f /etc/bashrc ] && . /etc/bashrc &> /dev/null

#
# Path
#

# Add user local bin/ directory to path
if [[ -e "$HOME/bin" ]]; then
    export PATH="$HOME/bin:$PATH"
fi

# Add ruby gems to path
export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"

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

###############################################################################
# If not running interactively, the rest of the script is not executed.
#
[[ $- != *i* ]] && return

#
# Set prompt
#

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
RC_PS1='$(RC="$?"; [[ "$RC" -ne 0 ]] && echo -n "[rc=$RC] ")'
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
export PS1="$RC_PS1$JOBS_PS1$LOGIN_PS1$WINDOW_PS1 $PWD_PS1$GIT_PS1\$ "
export SUDO_PS1="$( echo -n "$PS1" | sed -e 's/;32m/;31m/g' )"
unset color_prompt color_prompt_when_supported

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
if [ -e "/usr/bin/virtualenvwrapper.sh" ]; then
    . /usr/bin/virtualenvwrapper.sh
fi
