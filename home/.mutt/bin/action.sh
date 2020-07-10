#!/bin/bash

scripts=()
for script in $(find "$HOME/.mutt/action.d/" \
		     -maxdepth 1 -mindepth 1 \
		     -type f -a -executable | sort); do
	scripts+=( "$script" )
done

title="Pick an action:"
while true; do
	if which whiptail &> /dev/null; then
		options=()
		for i in "${!scripts[@]}"; do
			options+=( "$i" "$(basename "${scripts[$i]}")" )
		done
		answer=$(whiptail --notags --nocancel --menu "$title" 0 0 0 \
				  "${options[@]}" 3>&1 1>&2 2>&3)
	else
		echo "$title"
		for i in "${!scripts[@]}"; do
			echo "$i) ${scripts[$i]}"
		done
		read -p '> ' answer </dev/tty
	fi
	if [ -z "$answer" ]; then
		# Abort without change the input
		exit
	fi
	if [ -z "${scripts[$answer]}" ]; then
		echo "Invalid option."
		continue
	fi
	exec "${scripts[$answer]}" "$@"
	exit
done
