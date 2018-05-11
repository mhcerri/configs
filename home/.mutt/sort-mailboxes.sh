#!/bin/bash

PATTERNS=(
	'^INBOX$'
	'^\['
)

# Parser cmdline
if [[ $# -ne 1 ]]; then
	echo "Usage: $(basename "$0") <maildir>" >&2
	exit 1
fi
if [[ ! -d $1 ]]; then
	echo "Invalid maildir: $1" >&2
	exit 2
fi
dir="${1%/}/"

# List all mail dirs
cd "$dir"
IFS=$'\n'
mdirs=()
for mdir in $(find . -type d -name cur -printf '%h\n' |
		sed -e 's/^\.\///' |
		sort); do
	mdirs+=("$mdir")
done

# Sort given priority to the mail dirs that match the patterns
sorted=()
for p in "${PATTERNS[@]}"; do
	remaining=()
	for mdir in "${mdirs[@]}"; do
		if echo "$mdir" | grep -qEi "$p" &> /dev/null; then
			sorted+=("$mdir")
		else
			remaining+=("$mdir")
		fi
	done
	mdirs=("${remaining[@]}")
done
sorted+=("${mdirs[@]}")

# Print the final list
for d in "${sorted[@]}"; do
	printf '"+%s" ' "$d"
done
