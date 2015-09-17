#
# ~/.xinitrc
#

# Load X resources
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

# Load system scripts. This should handle D-Bus.
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Monitor calibration
[[ -f ~/.monicarc ]] && source ~/.monicarc

# Set environment
. ~/.bashrc
# TODO use a loop instead
if which terminology &> /dev/null; then
    export TERMINAL=terminology
elif which urxvt256c &> /dev/null; then
    export TERMINAL=urxvt256c
elif which mate-terminal &> /dev/null; then
    export TERMINAL=mate-terminal
fi

# Ensure that SSH will ask the passphase via console
export SSH_ASKPASS=

# Set X related settings
xsetroot -solid '#555555'

# Screensaver
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
rofi &

# Start window manager
i3

clear
