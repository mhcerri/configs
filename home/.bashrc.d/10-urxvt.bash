#
# TERM=rxvt-unicode-256color breaks a lot of application. Setting
# URxvt.termName:xterm-256color in ~/.Xresources also causes
# additional problems.
#
# As an workaround replace the TERM variable value here:
#
case "$TERM" in
rxvt*)
	export TERM=xterm-256color
	;;
esac
