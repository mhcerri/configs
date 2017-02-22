#!/bin/bash
orig="$(cat)"
date="$(echo "$orig" | formail -c -x "Date:")"
local_date="$(date -R -d "$date")"
echo "$orig" | formail -i "Date: $local_date"
