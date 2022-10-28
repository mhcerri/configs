#!/bin/bash
#
# Logger script for dunst.
#
# Example config for dunstrc:
#
# [logging]
# 	summary = "*"
# 	script = ~/.config/dunst/log.sh
#
# When the script is called details of the notification that triggered
# it will be passed via environment variables. The following variables
# are available: DUNST_APP_NAME, DUNST_SUMMARY, DUNST_BODY,
# DUNST_ICON_PATH, DUNST_URGENCY, DUNST_ID, DUNST_PROGRESS,
# DUNST_CATEGORY, DUNST_STACK_TAG, DUNST_URLS, DUNST_TIMEOUT,
# DUNST_TIMESTAMP, DUNST_DESKTOP_ENTRY, and DUNST_STACK_TAG.
#
# Another, less recommended way to get notifcations details from a
# script is via command line parameters. These are passed to the
# script in the following order: appname, summary, body, icon_path,
# urgency.
#
# https://github.com/dunst-project/dunst/blob/master/docs/dunst.5.pod#scripting
#

log_file=~/.local/var/dunst.log
max_lines=1000

# Log message
DUNST_BODY=$(echo "${DUNST_BODY}" | sed ':a;N;$!ba;s/\n/\\n/g')
echo "$(date -Is): ${DUNST_URGENCY} (${DUNST_APP_NAME:--}/${DUNST_DESKTOP_ENTRY:--}): ${DUNST_SUMMARY}: ${DUNST_BODY}" >> "${log_file}"

# Rotate the log:
tail -n "${max_lines}" "${log_file}" > "${log_file}.tmp"
mv "${log_file}.tmp" "${log_file}"
