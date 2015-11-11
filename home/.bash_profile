#
# ~/.bash_profile
#

if [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]]
then
	# There's a bug that doesn't allow redirecting the X output...
	exec startx 
	exit
fi

[[ -f ~/.bashrc ]] && . ~/.bashrc
