_id=
if command -v lsb_release &> /dev/null; then
	_id=$(lsb_release -i | awk -vFS=':\\s*' '{print $2}')
elif [ -f /etc/os-release ]; then
	source /etc/os-release
	_id="${ID:$ID_LIKE}"
fi
_id=$(echo "$_id" | tr '[:upper:]' '[:lower:]')
