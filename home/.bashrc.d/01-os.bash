_id=
if [ -f /etc/os-release ]; then
	source /etc/os-release
	_id="${ID:$ID_LIKE}"
elif command -v lsb_release &> /dev/null; then
	_id=$(lsb_release -i)
	_id="${_id##*:}"
	_id="${_id## }"
fi
_id=$(echo "$_id" | tr '[:upper:]' '[:lower:]')
