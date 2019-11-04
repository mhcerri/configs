#
# Set prompt
#
export _TERM=$(ps -o 'cmd=' -p $(ps -o 'ppid=' -p $$))
OLD_TERM="$TERM"
case "$_TERM" in
	terminology)
		export TERM=xterm-256color
		which emacs &> /dev/null && alias emacs='TERM=rxvt-unicode-256color emacs'
		which emacsclient &> /dev/null && alias emacsclient='TERM=rxvt-unicode-256color emacsclient'
		;;
	*terminator|mate-terminal)
		export TERM=xterm-256color
		;;
esac
[ $OLD_TERM == $TERM ] && unset OLD_TERM

# Uncomment for color prompt when supported
color_prompt_when_supported=yes

# Check for color support
if [ -n "$color_prompt_when_supported" ]; then
	if [ -x /usr/bin/tput ] && tput setaf 1 >& /dev/null; then
		color_prompt=yes
	fi
fi

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
	debian_chroot=$(cat /etc/debian_chroot)
fi

# Define prompt fragments
jobs_ps1='$(
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
debian_chroot_ps1='${debian_chroot:+($debian_chroot)}'
rc_ps1='$(RC="$?"; [[ "$RC" -ne 0 ]] && echo -n "[rc=$RC] ")'
login_ps1='\u@\h'
window_ps1="$([[ -n "$WINDOW" ]] && echo -n "[w$WINDOW]")"
pwd_ps1='\w'
git_ps1='$(__git_ps1 " (%s)" 2> /dev/null)'

# Add color to fragments
if [ "$color_prompt" = yes ]; then
	# No color for debian_chroot_ps1
	[ -n "$rc_ps1"     ] &&     rc_ps1="\[\033[01;31m\]$rc_ps1\[\033[01;00m\]"
	[ -n "$jobs_ps1"   ] &&   jobs_ps1="\[\033[01;32m\]$jobs_ps1\[\033[01;00m\]"
	[ -n "$login_ps1"  ] &&  login_ps1="\[\033[01;38;5;208m\]$login_ps1\[\033[00m\]"
	[ -n "$window_ps1" ] && window_ps1="\[\033[01;32m\]$window_ps1\[\033[00m\]"
	[ -n "$pwd_ps1"    ] &&    pwd_ps1="\[\033[01;34m\]$pwd_ps1\[\033[00m\]"
	[ -n "$git_ps1"    ] &&    git_ps1="\[\033[01;31m\]$git_ps1\[\033[01;00m\]"
fi

# Set prompt
export PS1="$debian_chroot_ps1$rc_ps1$jobs_ps1$login_ps1$window_ps1 $pwd_ps1$git_ps1\$ "
export SUDO_PS1="$( echo -n "$PS1" | sed -e 's/;32m/;31m/g' )"
export ALT_PS1="$PS1\[\a\]"
unset debian_chroot_ps1 rc_ps1 login_ps1 window_ps1 pwd_ps1 git_ps1 jobs_ps1
unset color_prompt color_prompt_when_supported

# Terminal title
case "$TERM" in
dumb|eterm*)
	# Do not set title for emacs terminals
	export PROMPT_COMMAND=
	;;
*)
	export PROMPT_COMMAND='echo -ne "\033]0;${USER}${HOSTNAME} ${PWD}\007"'
esac

# Bell the terminal when the prompt is shown
toggle_bell() {
	local ps1="$PS1"
	PS1="$ALT_PS1"
	ALT_PS1="$ps1"
}
