#!/bin/sh

params=/proc/asound/Pro/pcm0p/sub0/hw_params
echo -n "k5pro:"
{
	if [ -f "$params" ]; then
		cat "$params"
	else
		echo
	fi
} |
awk '
	/^format:/ {
	       match($2, /[A-Z]*([0-9]*).*/, arr)
	       bits=arr[1]
	}
	/^rate:/ { hz=$2 }
	END {
		if (!bits || !hz) {
			printf("off\n")
		} else {
			printf("%sbits@%d.1KHz\n", bits, hz/1000)
		}
	}
'
