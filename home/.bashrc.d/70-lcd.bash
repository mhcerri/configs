#
# Variables
#
_lcd_ws=~/workspace/linux/
_lcd_dir=git
_lcd_prefix=linux-

#
# Completion
#
_lcd() {
	local i cur prev opts
	cur="${COMP_WORDS[COMP_CWORD]}"
	prev="${COMP_WORDS[COMP_CWORD-1]}"
	opts=$(
		local IFS=$'\n'
		find "$_lcd_ws" -mindepth 2 -maxdepth 2 \
		     -name "$_lcd_dir" -printf '%h\n' |
			while read i
			do
				i="${i##*/}"
				if [[ $cur != $_lcd_prefix* ]]; then
					i="${i#$_lcd_prefix}"
				fi
				printf '"%q"\n' "$i"
			done
	    )
	local IFS=$'\n';
	COMPREPLY=( $(compgen -W "$opts" -- "$cur") )
	return 0
}
complete -F _lcd lcd

#
# Utility to jump to a kernel git repository directory
#
lcd() {
	local d
	for d in \
		"$_lcd_ws/$1/$_lcd_dir" \
		"$_lcd_ws/$_lcd_prefix$1/$_lcd_dir"
	do
		if [[ -d $d ]]; then
			cd "$d"
			return 0
		fi
	done
	echo "Invalid option: $1"
	return 1
}
