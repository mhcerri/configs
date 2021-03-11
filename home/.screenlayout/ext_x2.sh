#!/bin/sh
xrandr \
	--dpi 276 \
	--fb 7680x2160 \
	--output eDP-1-1 \
		--primary \
		--rotate normal \
		--pos 0x0 \
		--mode 1920x1080 \
		--scale 2x2 \
	--output HDMI-0 \
		--mode 3840x2160 \
		--pos 3840x0 \
		--rotate normal \
	--output DP-0 --off \
	--output DP-1 --off \
	--output DP-1-1 --off \
	--output HDMI-1-1 --off \
	--output DP-1-2 --off \
	--output HDMI-1-2 --off
