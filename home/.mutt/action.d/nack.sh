#!/bin/bash
realname=$(mutt -D 2> /dev/null | sed -n -r '/^realname="(.*)"/s//\1/p')
from=$(mutt -D 2> /dev/null | sed -n -r '/^from="(.*)"/s//\1/p')
sed -r \
	-e 's/(Subject:)\s*([Rr][Ee]:)?(.*)/\1 NACK:\3/' \
	-i "$*"
