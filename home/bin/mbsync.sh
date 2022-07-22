#!/bin/bash -eu

exec &> >(tee ~/.log/mbsync.log)

title="mbsync via mutt"

# Measure time
start_time=$(date +%s)
elapsed_secs() {
	local now
	now=$(date +%s)
	echo "$((now - start_time))"
}

log() {
	echo "$(date -Is) $*"
}

notify() {
	local type
	type="$1"
	shift

	args=()
	if [ "$type" = 'info' ]; then
		args+=('--urgency=low')
	fi

	log "$type: $*"
	if [ -n "$DISPLAY" ]; then
		notify-send "${args[@]}" -i "$type" "$title" "$*"
	fi
}

error() { notify error "$@"; }

info() { notify info "$@"; }

if ! command -v mbsync &> /dev/null; then
	error "mbsync is not installed!"
	exit 1
fi

log "Fetching email..."
if ! output=$(mbsync -V "$*" 3>&2 2>&1 1>&3- | tee >(cat >&2)); then
	error "Failed to fetch email ($(elapsed_secs) secs):"$'\n\n'"$output"
	exit 1
fi

info "Mail synchronization finished! ($(elapsed_secs) secs)"
exit 0
