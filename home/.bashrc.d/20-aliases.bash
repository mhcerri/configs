#
# Aliases
#
# Tips:
#
# 1. Use "complete -p <cmd>" to get the completion specification of a
#    command you want to create an alias for.
# 2. Use "_completion_loader <cmd>" to source the necessary completion
#    function.
#

alias ls='ls --color=auto'
alias ll='ls -l'
alias lt='ls -ltrh'

# Ranger
alias r='ranger'

# Jump to the root directory of a git repository
alias gcd='git rev-parse --show-toplevel 2> /dev/null && cd $(git rev-parse --show-toplevel) || echo "Not a git repo"'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Git
alias g=git
_completion_loader git
complete -o bashdefault -o default -o nospace -F __git_wrap__git_main g

if command -v bat &> /dev/null; then
	alias cat=bat
fi

# fdfind
alias fd=fdfind
