#
# Check if fzf is available
#
if ! command -v fzf &> /dev/null; then
	echo "Please install fzf..." >&2
else
	for _fzf_comp_script in \
		'/usr/share/doc/fzf/examples/completion.bash' \
			'/usr/share/bash-completion/completions/fzf'\
			'/usr/share/doc/fzf/examples/key-bindings.bash' \
			'/usr/share/doc/fzf/examples/completion.bash'; do
		if [ -f "$_fzf_comp_script" ]; then
			source "$_fzf_comp_script"
		fi
	done
fi


if command -v fd &> /dev/null; then
	# Use fd (https://github.com/sharkdp/fd) instead of the default find
	# command for listing path candidates.
	# - The first argument to the function ($1) is the base path to start traversal
	# - See the source code (completion.{bash,zsh}) for the details.
	_fzf_compgen_path() {
		fd --hidden --follow --exclude ".git" . "$1"
	}

	# Use fd to generate the list for directory completion
	_fzf_compgen_dir() {
		fd --type d --hidden --follow --exclude ".git" . "$1"
	}
fi

#
# Add fcd to easily change directories.
#
_fcd_config=~/.fcd.locations

_fcd_fzf_opts=
_fcd_fzf_opts+=" --height ${FZF_TMUX_HEIGHT:-50%}"
_fcd_fzf_opts+=" --min-height 15"
_fcd_fzf_opts+=" --reverse"
_fcd_fzf_opts+=" $FZF_DEFAULT_OPTS"
_fcd_fzf_opts+=" --preview 'ls -l {}' --preview-window right:50%:wrap"

fcd() {
	local dir opts

	if ! command -v fzf &> /dev/null; then
		echo "Warning: please install fzf!" >&2
	fi

	if [ -n "$*" ]; then
		cd "$*"
		return 0
	fi

	opts=$(fcd-locations-expanded)
	if [ -z "$opts" ]; then
		echo "Add entries to \"$_fcd_config\"..."
		return 1
	fi

	if dir=$(echo "$opts" | FZF_DEFAULT_OPTS="$_fcd_fzf_opts" fzf) && [ -d "$dir" ]; then
		cd "$dir"
		return 0
	fi

	echo "Aborted..."
	return 1
}

_fcd() {
	local fzf cur opts selected

	fzf="$(__fzfcmd_complete)"
	opts+="$_fcd_fzf_opts $FZF_COMPLETION_OPTS"
	cur="${COMP_WORDS[COMP_CWORD]}"
	selected=$(fcd-locations-expanded |
			   FZF_DEFAULT_OPTS="$opts" $fzf -m -q "$cur")
	printf '\e[5n'

	if [ -n "$selected" ]; then
		COMPREPLY=( "$selected" )
		return 0
	fi
}
complete -F _fcd fcd

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
