# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# Try to use any custom emacs version available at /opt
_opt_emacs=$(find /opt -mindepth 1 -maxdepth 1 -name 'emacs*' | sort | tail -n1)
if [[ -n $_opt_emacs ]]; then
	export PATH="$_opt_emacs/bin/:$PATH"
fi
unset _opt_emacs

export PATH="$HOME/.cargo/bin:$PATH"
