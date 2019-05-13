#
# Aliases
#

alias ls='ls --color=auto'
alias ll='ls -l'
alias lt='ls -ltrh'

# Jump to the root directory of a git repository
alias gcd='git rev-parse --show-toplevel 2> /dev/null && cd $(git rev-parse --show-toplevel) || echo "Not a git repo"'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
