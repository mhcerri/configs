#!/bin/bash
#
# Google calendar reminder.
#
# It's necessary to install gcalcli and to authenticate before using this
# script.
#
# Add it to user's crontab using crontab -e. Example:
#
#   DISPLAY=:0
#   */10 * * * * ~/bin/google-calendar-remider.sh
#

# Check if google calendar cli tools is installed
if ! which gcalcli &>/dev/null; then
    notify-send "Please install gcalcli"
    exit 1
fi

# Notification command
cmd='"notify-send           \
        -u critical         \
        -a gcalcli          \
        -i appointment-soon \
        --expire-time=0     \
        %s"'

# Check reminders
eval gcalcli remind "${1:-10}" "$cmd"
