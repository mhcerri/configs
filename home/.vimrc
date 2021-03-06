"
" Simple Vim configuration file.
"
" This file is intended to be kept simple and with few dependencies as possible.
"
" Notes:
"
" - Remember windows position:
"   http://www.vim.org/scripts/script.php?script_id=3626
"
" - Scala support:
"   mkdir -p ~/.vim/{ftdetect,indent,syntax} && for d in ftdetect indent syntax ; do curl -o ~/.vim/$d/scala.vim https://raw.githubusercontent.com/derekwyatt/vim-scala/master/syntax/scala.vim; done
"

set nocompatible

" Don't use the default leader key
let mapleader=";"

" Initialize Pathogen
silent! execute pathogen#infect()

" Jump to the last position when reopening a file
" Note: ~/.viminfo must be owned by you user
if has("autocmd")
	autocmd BufReadPost *
		\ if line("'\"") >= 1 && line("'\"") <= line("$") |
		\ 	 exe "normal! g`\"" |
		\ endif
endif

" File encoding
set encoding=utf-8
set fileencoding=utf-8

" Editing options
set autoindent
set noexpandtab
set backspace=2
set tabstop=8
set softtabstop=8
set shiftwidth=8
set smartindent
set hidden

if has ("title")
    " if there is no &t_ts sequence, is the terminal type known?
    if &t_ts == "" && ( &term == "screen" || &term == "xterm" || &term == "xterm-256color" )
        " add the missing control sequence for xterm or screen
        let &t_ts = "\e]2;"
    endif

    " enable setting the title only if vim can read the old one
    if &t_ts != ""
        set title
	set titleold=
    endif
endif

autocmd FileType C set
	\ setlocal textwidth=80

autocmd FileType javascript,scala set
	\ expandtab
	\ tabstop=2
	\ softtabstop=2
	\ shiftwidth=2

autocmd Filetype make set
	\ noexpandtab
	\ tabstop=8
	\ softtabstop=8
	\ shiftwidth=8

autocmd Filetype python set
	\ expandtab
	\ tabstop=4
	\ softtabstop=4
	\ shiftwidth=4

" Git conflicts
nmap <Leader>c /^\(=======\\|<<<<<<<\\|>>>>>>>\)\(\s.*\)\?$<CR>
autocmd ColorScheme * highlight GitConflicts ctermbg=red ctermfg=white guibg=red guifg=white
highlight GitConflicts ctermbg=red ctermfg=white guibg=red guifg=white
match GitConflicts /^\(=======\|<<<<<<<\|>>>>>>>\)\(\s.*\)\?$/

" Syntax, color and highlight
filetype plugin indent on
syntax on
set number
set hlsearch
try
	" Inverse priority list of preferred color schemes:
	colorscheme industry
	highlight ColorColumn ctermbg=232 ctermfg=1
	" git clone https://github.com/endel/vim-github-colorscheme.git ~/.vim/bundle/vim-github-colorscheme
	if &t_Co >= 256
		colorscheme github
	endif
catch
endtry
if has('gui_running')
	" Add gvim specific configs here:
	"set guifont=Consolas:h10
endif

if &term =~ '256color'
	" disable Background Color Erase (BCE) so that color schemes
	" render properly when inside 256-color tmux and GNU screen.
	" see also http://snk.tuxfamily.org/log/vim-256color-bce.html
	set t_ut=
endif

" Show tabs and other special characters
"set list
"set listchars=tab:␉·,trail:␠,nbsp:⎵

" Spell
" cd ~/.vim/spell/; wget 'http://ftp.vim.org/pub/vim/runtime/spell/pt.utf-8.spl'
set spelllang=en,pt
silent! set spell
hi clear SpellBad
hi Spellbad
	\ term    = underline
	\ cterm   = underline
	\ gui     = underline
	\ ctermbg = NONE
	\ ctermfg = NONE
	\ guibg   = NONE
	\ guifg   = NONE

" Search tag files in parent directories
set tags=tags;

" :w!!
" Write the file when you accidentally opened it without the right (root) privileges
cmap w!! w !sudo tee % > /dev/null

" Buffer shortcuts
nmap <Tab> :silent! bnext<CR>
nmap <S-Tab> :silent! bprevious<CR>
map <C-n> :silent! bnext<CR>
map <C-p> :silent! bprevious<CR>
nmap <Leader>q :silent! bdelete<CR>

" Tabs shortcuts
nmap . :tabnext<CR>
nmap , :tabprevious<CR>
nmap > :tabmove +1<CR>
nmap < :tabmove -1<CR>

" Stay in visual mode when shifting
vnoremap < <gv
vnoremap > >gv

" Open .vimrc
nmap <Leader>v :tabe ~/.vimrc<CR>

" Paste mode toggle
set pastetoggle=<Ins>
nmap <Ins> :set paste<CR>i

" Re-wrap paragraph
nmap <Leader>w {gq}

" Scroll the screen before the cursor reaches edge
set scrolloff=5

" Mark limit column
execute "set colorcolumn=" . join(range(81,120), ',')

" Enable the mouse
set mouse=a
function! ToggleMouse()
	if &mouse == 'a'
		set mouse=
	else
		set mouse=a
	endif
endfunction
map <F12> :call ToggleMouse()<CR>

" Snippets
au BufNewFile,BufRead *.md iabbrev ''' ```
au FileType go abbrev ife if err != nil {<Enter>

" Git snippets
augroup FileType gitcommit
	iabbrev buglink BugLink: http://bugs.launchpad.net/bugs/
	iabbrev sob Signed-off-by: |
	iabbrev ack Acked-by:
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Pathogen bundles
" -----------------------------------------------------------------------------
" To install the bundles listed in this config file:
"
"   grep -E '^\s*"\s*git clone' ~/.vimrc | sed -e 's/^\s*"\s*//'
"
" All bundle configuration must be placed in SetBundleOptions() to be set
" lazily and must always check if the bundle is installed.

function! SetBundleOptions()
	""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
	" CsApprox - Better support with colorschemes
	" git clone https://github.com/godlygeek/csapprox.git ~/.vim/bundle/csapprox

	""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
	" Jedi-vim - Python support
	" git clone --recursive https://github.com/davidhalter/jedi-vim.git ~/.vim/bundle/jedi-vim

	""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
	" Vim buftabline - List buffer in the tab line
	" git clone https://github.com/ap/vim-buftabline ~/.vim/bundle/vim-buftabline

	""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
	" Syntastic - Check syntax error
	" git clone https://github.com/scrooloose/syntastic.git ~/.vim/bundle/syntastic
	if exists(':SyntasticCheck')
		set statusline+=%#warningmsg#
		set statusline+=%{SyntasticStatuslineFlag()}
		set statusline+=%*
		let g:syntastic_always_populate_loc_list = 1
		let g:syntastic_auto_loc_list = 0
		let g:syntastic_check_on_open = 1
		let g:syntastic_check_on_wq = 0
		let g:syntastic_auto_jump = 0
		let g:syntastic_java_checkers=['']
		let g:syntastic_c_checkpatch_exec = '~/bin/checkpatch.pl'
		let g:syntastic_c_checkers = ['checkpatch', 'gcc']
		let g:syntastic_go_checkers = ['govet', 'errcheck', 'go']
		" Cycle through errors
		function! LocationNext()
			try
				lnext
			catch /:E553:/
				lfirst
			catch /:E\%(42\|776\):/
				echo "Location list empty"
			catch /.*/
				echo v:exception
			endtry
		endfunction
		nmap <F2> :call LocationNext()<CR>
	endif

	""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
	" Neocomplete - Code completion
	" git clone https://github.com/Shougo/neocomplete.vim.git ~/.vim/bundle/neocomplete.vim
	if exists(':NeoCompleteEnable')
		" Disable AutoComplPop.
		let g:acp_enableAtStartup = 0
		" Use neocomplete.
		let g:neocomplete#enable_at_startup = 1
		" Use smartcase.
		let g:neocomplete#enable_smart_case = 1
		" Set minimum syntax keyword length.
		let g:neocomplete#sources#syntax#min_keyword_length = 3

		" Plugin key-mappings.
		inoremap <expr><C-g> neocomplete#undo_completion()
		inoremap <expr><C-l> neocomplete#complete_common_string()

		" Recommended key-mappings.
		" <CR>: close popup and save indent.
		inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
		function! s:my_cr_function()
			return neocomplete#close_popup() . "\<CR>"
		endfunction
		" <TAB>: completion.
		inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
		" <C-h>, <BS>: close popup and delete backword char.
		inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
		inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
		inoremap <expr><C-y>  neocomplete#close_popup()
		inoremap <expr><C-e>  neocomplete#cancel_popup()

		" Auto close top window preview
		let g:neocomplete#enable_auto_close_preview=1
		" Disable window preview
		set completeopt-=preview
	endif

	""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
	" Vim-go - Golang support
	" git clone https://github.com/fatih/vim-go.git ~/.vim/bundle/vim-go
	"
	" Check https://github.com/fatih/vim-go
	" Use :GoInstallBinaries to install all the needed binaries.
	"
	" Set gocode autobuild for updated autocompletion:
	" $ gocode set autobuild true
	"
	if exists(':GoPath')
		" Shortcuts:
		au FileType go nmap <Leader>r <Plug>(go-run)
		au FileType go nmap <Leader>b <Plug>(go-build)
		au FileType go nmap <Leader>t <Plug>(go-test)
		au FileType go nmap <Leader>c <Plug>(go-coverage)
		au FileType go nmap <Leader>ds <Plug>(go-def-split)
		au FileType go nmap <Leader>dv <Plug>(go-def-vertical)
		au FileType go nmap <Leader>dt <Plug>(go-def-tab)
		au FileType go nmap <Leader>gd <Plug>(go-doc)
		au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)
		au FileType go nmap <Leader>gb <Plug>(go-doc-browser)
		au FileType go nmap <Leader>s <Plug>(go-implements)
		au FileType go nmap <Leader>i <Plug>(go-info)
		au FileType go nmap <Leader>e <Plug>(go-rename)
		" Emulate tags:
		au FileType go nmap <C-]> <Plug>(go-def)

		" Highlights:
		let g:go_highlight_types = 1
		let g:go_highlight_functions = 1
		let g:go_highlight_methods = 1
		let g:go_highlight_structs = 1
		let g:go_highlight_interfaces = 1
		let g:go_highlight_operators = 1
		let g:go_highlight_build_constraints = 1

		" Go fmt with organize imports:
		let g:go_fmt_command = "goimports"
	endif

	""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
	" Nerd tree - Browse files
	" git clone https://github.com/scrooloose/nerdtree.git ~/.vim/bundle/nerdtree
	if exists(':NERDTree')
		" Shortcut:
		nmap <Leader>; :NERDTreeToggle<CR>
		au FileType nerdtree nmap <buffer> <left> x
		au FileType nerdtree nmap <buffer> <right> o
		" Close vim when Nerd tree is the last window
		autocmd bufenter *
			\ if (winnr("$") == 1
			\ && exists("b:NERDTree")
			\ && b:NERDTree.isTabTree()) | q | endif
		" General
		let NERDTreeDirArrows=1
		let NERDTreeMinimalUI=1
		let NERDTreeIgnore=['\.o$', '\.pyc$', '\.php\~$']
		let NERDTreeWinSize = 30
		let NERDTreeQuitOnOpen=1
		let NERDTreeCascadeOpenSingleChildDir=1
	endif

	""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
	" git clone https://github.com/vivien/vim-linux-coding-style.git ~/.vim/bundle/vim-linux-coding-style
	"
	if exists(':LinuxCodingStyle')
		let g:linuxsty_patterns = [ "xenial", "linux"  ]
	endif
endfunction
au VimEnter * call SetBundleOptions()
