#!/bin/bash
realname=$(mutt -D 2> /dev/null | sed -n -r '/^(set\s+)?realname\s*=\s*"(.*)"/s//\2/p')
from=$(mutt -D 2> /dev/null | sed -n -r '/^(set\s+)?from\s*=\s*"(.*)"/s//\2/p')
awk -vrealname="$realname" -vfrom="$from" '
	!edited && /^$/ {
		printf("\nNacked.\n");
		edited = 1;
	}
	/^Subject:/ {
		    $0 = gensub(/(Subject:)\s*([Rr][Ee]:)?(.*)/, "\\1 NACK:\\3", 1, $0);
	}
	{
		print;
	}
' "${*}" > "${*}.tmp"
mv "${*}.tmp" "${*}"
