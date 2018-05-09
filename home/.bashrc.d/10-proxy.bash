#
# Proxy settings
#
if [[ -z "$proxy_addr" ]]; then
	proxy_file="$HOME/.proxy"
	if [[ -f "$proxy_file" ]]; then
		proxy_addr=$( cat "$proxy_file" )
	fi
fi
if [[ -n "$proxy_addr" ]]; then
	export http_proxy="$proxy_addr"
	export https_proxy="$proxy_addr"
fi
unset proxy_addr proxy_file
