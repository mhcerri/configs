#
# todo.txt
#
alias t=todo-txt
declare -f _todo 2>&1 >/dev/null ||
for f in \
	/usr/share/bash-completion/completions/todo-txt
do
	if [ -f "$f" ]; then
		source "$f"
		break
	fi
done
if declare -f _todo 2>&1 >/dev/null; then
	complete -F _todo t
fi
