#
# Emacs
#

# Note: use ~/.profile or a similar file to add a customized emacs to
#       the PATH.

# Kill the muscle memory
alias vim='echo -e "\033[1;31mTry \`e\` or \`v\`...\033[0m"'

# Useful aliases
# not need for "alies e=" because I have a helper script for emacsclient
# already named "e".
alias emacs='emacs -nw --no-site-file'
alias v=lvim

# Add custom emacs to the path
export PATH="$HOME/.local/share/emacs/bin/:$PATH"
