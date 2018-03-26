#!/bin/bash
if which emacsclient &> /dev/null; then
	emacsclient -a "" -t "$@"
else
	vim				\
		-c 'silent! /^$'	\
		-c 'let @/ = ""'	\
		-c 'set textwidth=72'	\
		-c 'set wrap'		\
		-c 'set nocp'		\
		-c 'set spell'		\
		"$@"
fi
