#!/bin/sh
xrandr \
	--output HDMI-0 \
		--primary \
		--mode 3840x2160 \
		--pos 1920x0 \
		--rotate normal \
	--output eDP-1-1 \
		--scale 1x1 \
		--mode 1920x1080 \
		--pos 0x0 \
		--rotate normal \
	--output DP-0 --off \
	--output DP-1 --off \
	--output DP-1-1 --off \
	--output HDMI-1-1 --off \
	--output DP-1-2 --off \
	--output HDMI-1-2 --off
