#!/bin/sh
echo "$*" > ~/.rofi-last-cmd
exec "$@"
