#!/bin/sh
URL='http://www.pragmatismopolitico.com.br/2014/09/os-253-candidatos-ficha-suja-nas-eleicoes-2014.html'
curl "$URL" 2> /dev/null |
html2text |
iconv -f utf-8 -t ascii//translit |
tr '[:lower:]' '[:upper:]' |
grep '|' |
perl -p -e 's/\n/ /g' |
sed -e 's/|/ /g' |
sed -e 's/\s*-\s*/-/g' |
sed -e 's/\s\s*/\n/g' |
sed -e 's/-/ /g' |
awk '
 { t[$1] += $2; }
 END { for (p in t) { print p, t[p]; }; }
' |
sort -n -k 2 -r
