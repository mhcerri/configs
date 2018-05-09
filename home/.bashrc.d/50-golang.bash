#
# Golang
#

# Add Go user dirs to path
if which go &> /dev/null; then
	export GOPATH="$HOME/go"
	export PATH="$PATH:$GOPATH/bin"
	[[ ! -e "$GOPATH" ]] && mkdir -p "$GOPATH"
fi

# Alias to set GOPATH as the current directory
alias gopath='
	export GOPATH="$PWD";
	export GO15VENDOREXPERIMENT=1;
	echo "GOPATH=\"$GOPATH\"";
	echo "GO15VENDOREXPERIMENT=\"$GO15VENDOREXPERIMENT\"";
'
