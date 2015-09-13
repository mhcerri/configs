#!/bin/bash

INPUT_DEV_DIR=/dev/input
SYS_DEV_DIR=/sys/devices
FILTER='Gamepad'

EVS=''
_IFS="$IFS"
IFS='
'
for ENTRY in `find "$SYS_DEV_DIR" -name 'js*'`; do
	JS="`basename "$ENTRY"`"
	DEV="$ENTRY/device/"
	NAME_PATH="$DEV/name"

	echo -n "Found joystick '$JS': "

	if [[ ! -d "$DEV" ]] || [[ ! -e "$NAME_PATH" ]]; then
		echo -e "invalid joystick\n"
		continue
	fi

	NAME="`cat "$NAME_PATH"`"
	echo "$NAME"

	if [[ ! "$NAME" =~ "$FILTER" ]]; then
		echo -e "  Skipping '$JS'\n"
		continue
	fi

	for EV in `find "$DEV" -maxdepth 1 -name 'event*'`; do
		EV="`basename "$EV"`"
		EV_DEV="$INPUT_DEV_DIR/$EV"
		if [[ ! -e "$EV_DEV" ]]; then
			echo "  Skipping invalid event '$EV'"
		else
			echo "  Adding event '$EV' for joystick '$JS'"
			EVS="`echo -e "$EVS\n$EV_DEV"`"
		fi
	done
	echo
done

if [[ -z "$EVS" ]]; then
	echo "No joysticks available"
	exit 1
fi

echo "Using default mapping for: "$EVS
echo

SUDO=""
if [[ "`id -u`" -ne 0 ]]; then
	SUDO=sudo
fi

$SUDO xboxdrv								\
	--evdev $EVS 							\
	--axismap -Y1=Y1,-Y2=Y2						\
	--evdev-absmap ABS_X=x1,ABS_Y=y1,ABS_RZ=y2,ABS_Z=x2		\
	--evdev-absmap ABS_HAT0X=dpad_x,ABS_HAT0Y=dpad_y		\
	--evdev-keymap BTN_X=x,BTN_Y=y,BTN_A=a,BTN_B=b			\
	--evdev-keymap BTN_SELECT=back,BTN_START=start			\
	--evdev-keymap BTN_TL2=lt,BTN_TR2=rt				\
	--evdev-keymap BTN_TL=lb,BTN_TR=rb				\
	--evdev-keymap BTN_THUMBL=tl,BTN_THUMBR=tr			\
	--mimic-xpad							\
	--force-feedback						\
	--silent

