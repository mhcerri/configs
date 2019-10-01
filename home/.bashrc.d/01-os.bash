_id=$(lsb_release -i |
	      awk -vFS=':\\s*' '{print $2}' |
	      tr '[:upper:]' '[:lower:]')
