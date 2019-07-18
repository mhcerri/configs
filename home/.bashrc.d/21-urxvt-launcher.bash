# Based on https://github.com/BenKerry/dots/tree/master/urxvt-launcher
# Use an env var as a signal to enable the urxvt launcher behavior:
if [ -n "$_LAUNCHER" ]; then
	unset _LAUNCHER
	# Set a custom prompt:
	PS1="\[\033[01;34m\]run:\[\033[00m\] "
	# Move the cursor to the end of the line, add &, hit enter to run the
	# command and exit the shell:
	bind 'RETURN: "\e[4~ & \n exit \n"'
	return
fi
