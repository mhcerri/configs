#!/bin/bash

# ./script appname summary body icon urgency
appname="$1"
summary="$2"
body="$3"
icon="$4"
urgency="$5"

tag='↩️'

if echo "$summary" | grep -F "$tag" &> /dev/null; then
	echo "Skipping tagged notification."
	exit 0
fi

notify-send \
	-i "$icon" \
	-u "$urgency" \
	"$summary $tag" \
	"$body"
