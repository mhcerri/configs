#!/bin/bash -x

#
#  Main monitor     External monitor
#
# 0,0
#  .--------------. .-------------------------.
#  |1        |    | |2                        |
#  |         |    | |                         |
#  |---------+    | |                         |
#  |             *| |                         |
#  '--------------' |                         |
#                   |                         |
#                   |                         |
#                   '-------------------------'
#
# 1. Main monitor size
# 2. External monitor size
# *. Effective main monitor size
#

# TODOs:
# 1. Try to get the output names automatically
# 2. Try to get the best mode for each output
# 3. Set the position of the main monitor

scale="${1:-1.5}"

# Main monitor
out1=eDP-1-1
w1=1920
h1=1080

# External monitor
#out2=HDMI-0
out2=DP-0
w2=3840
h2=2160

# Effective main monitor
ew1=$(echo "($w1 * $scale)/1" | bc)
eh1=$(echo "($h1 * $scale)/1" | bc)

# Set the main display separate to avoid issues if the external
# display is disconnected.
xrandr --output "$out1" \
		--primary \
		--pos 0x0 \
		--mode "${w1}x${h1}" \
		--scale "${scale}x${scale}" ||
	echo "Failed to set main monitor mode." >&2

#Set the external display
xrandr --output "$out2" \
		--mode "${w2}x${h2}"\
		--pos "${ew1}x0" ||
	echo "Failed to set external monitor mode." >&2

# Set prime sync
xrandr --output "$out1" --set "PRIME Synchronization" 1

# Set Nvidia composition here since the /etc/X11/xorg.conf file
# written by nvidia-settings doesn't work.
if ! command -v nvidia-settings >/dev/null 2>&1; then
	echo "Cannot find nvidia-settings." >&2
else
	nvidia_mode=
	#nvidia_mode+="CurrentMetaMode=DPY-0: nvidia-auto-select "
	nvidia_mode+="CurrentMetaMode=nvidia-auto-select "
	nvidia_mode+="@${w2}x${h2} +${ew1}+0 { "
	nvidia_mode+="ViewPortIn=${w2}x${h2}, "
	nvidia_mode+="ViewPortOut=${w2}x${h2}+0+0, "
	nvidia_mode+="ForceCompositionPipeline=On, "
	nvidia_mode+="ForceFullCompositionPipeline=On}"
	nvidia-settings --assign "$nvidia_mode"
fi

xrandr --output HDMI-0 --off
exit 0
