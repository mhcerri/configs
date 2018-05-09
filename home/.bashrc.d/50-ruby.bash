#
# Ruby
#

# Add ruby gems to path
if which ruby &>/dev/null; then
	export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
fi
