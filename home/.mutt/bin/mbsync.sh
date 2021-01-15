#!/bin/bash -eu

title="mbsync via mutt"

notify() {
	local type
	type="$1"
	shift

	if [ -n "$DISPLAY" ]; then
		notify-send -i "$type" "$title" "$*"
	else
		echo "$type: $*" >&2
	fi
}

error() { notify error "$@"; }

info() { notify info "$@"; }

if ! command -v mbsync &> /dev/null; then
	error "mbsync is not installed!"
	exit 1
fi

if ! output=$(mbsync "$*" 2>&1); then
	error "Failed to fetch email:"$'\n\n'"$output"
	exit 1
fi

info "Mail synchronization finished!"
exit 0
