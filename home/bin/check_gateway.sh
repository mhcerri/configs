#!/bin/bash

err() {
	echo -ne '\a'
	echo "$(date '+%Y-%m-%d %H:%M:%S'): $*" >&2
}

while true; do
	gw=$(ip route | sed -n '/^default via \([0-9.]\+\).*/s//\1/p' | head -n1)
	if [ -z "$gw" ]; then
		err "no gateway"
	else
		if !ping -q -n -c 1 -W 1 "$gw" &>/dev/null; then
			err "ping to $gw failed"
		fi
	fi
	sleep 1
done
