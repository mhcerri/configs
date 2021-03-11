#!/bin/bash -e

DEBUG=0
RATELIMIT=3 # secs


info () { echo "$*" >&2; }

debug() { if [ "$DEBUG" -ne 0 ]; then echo "DEBUG: $*" >&2; fi; }

notify() {
	if command -v notify-send &> /dev/null; then
		notify-send "Monitor" "$*"
	fi
	info "Notification: $*"
}

main_loop() {
	local devpath line key value
	debug "Entering main_loop"

	devpath=
	while read -r line; do
		# Empty line means end of an event info
		if [ -z "$line" ]; then
			debug "Processing event"
			process_event "$devpath"
			# reset state variables
			devpath=
			continue
		fi

		# Ignore lines without key and values
		[[ "$line" != *=* ]] && continue

		# Parse values
		key="${line%%=*}"
		value="${line#*=}"
		debug "Parsed: key=\"$key\", value=\"$value\""
		case "$key" in
			DEVPATH)
				devpath="$value"
				;;
			*)
				;;
		esac

		# Show udev events
	done < <(udevadm monitor -u -p -s drm)
}

process_event() {
	local devpath
	devpath="$1"

	debug "Entering process_event $*"

	# Skip invalid events (probably the header)
	if [ -z "$devpath" ]; then
		debug "Invalid devpath. Skipping"
		return
	fi

	update_sysfs_status "$sys/$devpath"
}

update_sysfs_status() {
	local path file i new old
	path=$(readlink -f "$1")

	debug "Entering update_sysfs_status $*"
	while read -r file; do
		# Get new and old values
		i=$(dirname "$file")
		new=$(cat "$file")
		old="${g_status[$i]}"
		debug "Status: $i $old $new"

		# Check if status has changed
		if [ -n "$old" ] && [ "$new" != "$old" ]; then
			# Call the trigger only on connection
			debug "Status change: $i $old $new"
			case "$new" in
				connected)
					call_trigger "$1" "$old" "$new"
					;;
				*)
					debug "Skipping trigger on disconnection"
					;;
			esac
		fi

		# Update status
		g_status["$i"]="$new"
	done < <(find "$path" -name status)
}

call_trigger() {
	local i old new now last

	i="$1"; old="$2"; new="$3"
	last="${g_last[$i]}"
	now=$(date +%s)

	debug "Entering call_trigger $* [last=$last, now=$now]"

	[ -z "$last" ] && last=0
	if (( now - last < RATELIMIT )); then
		info "Skipping trigger. Too soon"
		return
	fi

	notify "Calling trigger: ${g_trigger[*]}"
	"${g_trigger[@]}"

	g_last["$i"]="$now"
}

exit_trap() {
	notify "Exiting monitor (rc=$?)"
}
trap exit_trap 0


declare -A g_last
declare -A g_status
g_trigger=

# Check arguments
if [ "$#" -eq 0 ]; then
	echo "Usage: $(basename "$0") <callback_cmd>"
	exit 1
fi
g_trigger=("$@")

# Check access to sysfs
read -r _ _ sys _ < <(mount -l -t sysfs)
debug "sysfs mounted at $sys"
if [ ! -r "$sys" ]; then
	echo "Cannot read $sys"
	exit 1
fi

# Update status
while read -r drm; do
	update_sysfs_status "$drm"
done < <(find "$sys" -name drm -a -type d 2> /dev/null)

main_loop
