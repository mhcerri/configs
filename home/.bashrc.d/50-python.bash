#
# Python
#
export PYTHONSTARTUP="$HOME/.pyrc"
if [[ ! -e "$PYTHONSTARTUP" ]]; then
cat > "$PYTHONSTARTUP" << END
# Auto generated by ~/.bashrc
try:
	import readline
except ImportError:
	print("Module readline not available.")
else:
	import rlcompleter
	readline.parse_and_bind("tab: complete")
END
fi
if [ -e "/usr/bin/virtualenvwrapper.sh" ]; then
	. /usr/bin/virtualenvwrapper.sh
fi
