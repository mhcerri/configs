#
# Check if fzf is available
#
if ! command -v fzf &> /dev/null; then
	echo "Please install fzf..." >&2
elif ! [ -f "/usr/share/bash-completion/completions/fzf" ]; then
	echo "Missing fzf bash completion script..."
else
	source /usr/share/bash-completion/completions/fzf
	source /usr/share/doc/fzf/examples/key-bindings.bash
fi


#
# Add fcd to easily change directories.
#
_fcd_config=~/.fcd.locations

fcd() {
	local dir opts

	if ! command -v fzf &> /dev/null; then
		echo "Warning: please install fzf!" >&2
	fi

	opts=$(fcd-locations-expanded)
	if [ -z "$opts" ]; then
		echo "Add entries to \"$_fcd_config\"..."
		return 1
	fi

	if dir=$(echo "$opts" | fzf) && [ -d "$dir" ]; then
		cd "$dir"
		return 0
	fi

	echo "Aborted..."
	return 1
}

fcd-add() {
	local target
	if [ "$#" -eq 0 ]; then
		target="$PWD"
	else
		target="$*"
	fi
	echo "Adding: $target"
	echo "$target" >> "$_fcd_config"
}

fcd-locations() {
	if [ ! -e "$_fcd_config" ]; then
		return
	fi
	cat "$_fcd_config"
}

fcd-locations-expanded() {
	fcd-locations |
		while read -r line; do
			case "$line" in
			!*)
				cmd="${line#\!}"
				sh -c "$cmd"
				;;
			*)
				echo "$line"
				;;
			esac
		done |
		while read -r path; do
			[ -d "$path" ] && echo "$path"
		done |
		sort
}
