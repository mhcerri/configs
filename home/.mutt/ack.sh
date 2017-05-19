#!/bin/bash
sed -r \
	-e '0,/^$/!d' \
	-e 's/(Subject:)\s*([Rr][Ee]:)?(.*)/\1 ACK:\3/' \
	-i "$*"
