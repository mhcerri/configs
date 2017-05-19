#!/bin/bash
vim				\
	-c 'silent! /^$'	\
	-c 'let @/ = ""'	\
	-c 'set textwidth=72'	\
	-c 'set wrap'		\
	-c 'set nocp'		\
	-c 'set spell'		\
	"$@"
