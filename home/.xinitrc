#
# ~/.xinitrc
#
$HOME/.monicarc   #monitor calibration

# Load system scripts. This should handle D-Bus.
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Set environment
#export TERMINAL=mate-terminal
export TERMINAL=terminology

# Set X related settings
xrdb -load "$HOME/.Xresources"
xsetroot -solid '#555555'

# Screensaver
#xautolock -time 10 -locker 'i3lock -c 000000' &
xautolock -detectsleep -time 10 -locker '~/.i3/lock -c 000000' &
~/.i3/xautolock-dbus &

# Run Mate utils
/usr/lib/mate-settings-daemon/mate-settings-daemon &
/usr/lib/mate-polkit/polkit-mate-authentication-agent-1 &
mate-power-manager &
mate-bluetooth-applet &
nm-applet &

# Misc
kalu &
clipit &
psensor &

# Start window manager
i3

clear
