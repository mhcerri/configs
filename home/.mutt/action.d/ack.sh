#!/bin/bash
realname=$(mutt -D 2> /dev/null | sed -n -r '/^realname="(.*)"/s//\1/p')
from=$(mutt -D 2> /dev/null | sed -n -r '/^from="(.*)"/s//\1/p')
sed -r \
	-e $'$a\\\nAcked-by: '"$realname"' <'"$from"'>' \
	-e '0,/^$/!d' \
	-e 's/(Subject:)\s*([Rr][Ee]:)?(.*)/\1 ACK:\3/' \
	-i "$*"
