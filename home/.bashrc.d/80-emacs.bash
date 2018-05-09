#
# Emacs
#

# Try to use any custom emacs version available at /opt
_opt_emacs=$(find /opt -mindepth 1 -maxdepth 1 -name 'emacs*' | sort | tail -n1)
if [[ -n $_opt_emacs ]]; then
	export PATH="$_opt_emacs/bin/:$PATH"
fi
unset _opt_emacs

# Use emacs instead of vim
if which e &> /dev/null; then
	alias vim=e
fi
