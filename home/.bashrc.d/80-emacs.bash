#
# Emacs
#

# Note: use ~/.profile or a similar file to add a customized emacs to
#       the PATH.

# Use emacs instead of vim
if which e &> /dev/null; then
	alias vim=e
fi
alias emacs='emacs -nw'
