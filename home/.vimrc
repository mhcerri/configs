"
" Simple Vim configuration file.
"
" This file is intended to be kept simple and with few dependencies as possible.
"
" Notes:
"
" - Remember windows position:
"       http://www.vim.org/scripts/script.php?script_id=3626
"
" - Scala support:
"       mkdir -p ~/.vim/{ftdetect,indent,syntax} && for d in ftdetect indent syntax ; do curl -o ~/.vim/$d/scala.vim https://raw.githubusercontent.com/derekwyatt/vim-scala/master/syntax/scala.vim; done
"

" Don't use the default leader key
let mapleader=";"

" Initialize Pathogen
execute pathogen#infect()

" File encoding
set encoding=utf-8
set fileencoding=utf-8

" Editing options
set autoindent
set expandtab
set backspace=2
set tabstop=4
set softtabstop=4
set shiftwidth=4
set smartindent
set title
set hidden

autocmd FileType javascript,scala set
    \ tabstop=2
    \ softtabstop=2
    \ shiftwidth=2

autocmd Filetype make set
    \ noexpandtab

" Syntax, color and highlight
filetype plugin indent on
syntax on
set number
set hlsearch 
try
    " Inverse priority list of preferred color schemes:
    colorscheme industry
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

" Buffer shortcuts
:nnoremap <Tab> :silent! bnext<CR>
:nnoremap <S-Tab> :silent! bprevious<CR>
:nnoremap <Leader>q :silent! bdelete<CR>

" Tabs shortcuts
:nnoremap . :tabnext<CR>
:nnoremap , :tabprevious<CR>
:nnoremap > :tabmove +1<CR>
:nnoremap < :tabmove -1<CR>

" Stay in visual mode when shifting
vnoremap < <gv
vnoremap > >gv

" Open .vimrc
nmap <Leader>v :tabe ~/.vimrc<CR>

" Paste mode toggle
set pastetoggle=<Ins>
nmap <Ins> :set paste<CR>i

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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Pathogen bundles
" -----------------------------------------------------------------------------
" To install the bundles listed in this config file:
"
"   grep -E '^"\s*git clone' ~/.vimrc | sed -e 's/^"\s*//'
"
" Bundles configuration should start with "silent!" to avoid errors when
" they are not installed.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Auto-pair - Include or remove matching bracket and parenthesis
" git clone git://github.com/jiangmiao/auto-pairs.git ~/.vim/bundle/auto-pairs

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CsApprox - Better support with colorschemes
" git clone https://github.com/godlygeek/csapprox.git ~/.vim/bundle/csapprox

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim bufferline - List buffer in the command line
" git clone https://github.com/bling/vim-bufferline ~/.vim/bundle/vim-bufferline
silent! let g:bufferline_echo = 1
silent! let g:bufferline_fixed_index = 0
silent! let g:bufferline_rotate = 1
silent! let g:bufferline_show_bufnr = 1
silent! let g:bufferline_active_buffer_left = '['
silent! let g:bufferline_active_buffer_right = ']'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Syntastic - Check syntax error
" git clone https://github.com/scrooloose/syntastic.git ~/.vim/bundle/syntastic
silent! set statusline+=%#warningmsg#
silent! set statusline+=%{SyntasticStatuslineFlag()}
silent! set statusline+=%*
silent! let g:syntastic_always_populate_loc_list = 1
silent! let g:syntastic_auto_loc_list = 0
silent! let g:syntastic_check_on_open = 1
silent! let g:syntastic_check_on_wq = 0
silent! let g:syntastic_auto_jump = 0
silent! let g:syntastic_java_checkers=['']
" Cycle through errors
nnoremap <F2> :try<CR>lnext<CR>catch /E42/<CR>catch /E553/<CR>lfirst<CR>endtry<CR><CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Neocomplete - Code completion
" git clone https://github.com/Shougo/neocomplete.vim.git ~/.vim/bundle/neocomplete.vim
"  silent! let g:neocomplete#enable_at_startup = 1
"  silent! inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" Disable AutoComplPop.
silent! let g:acp_enableAtStartup = 0
" Use neocomplete.
silent! let g:neocomplete#enable_at_startup = 1
" Use smartcase.
silent! let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
silent! let g:neocomplete#sources#syntax#min_keyword_length = 3

" Plugin key-mappings.
silent! inoremap <expr><C-g>     neocomplete#undo_completion()
silent! inoremap <expr><C-l>     neocomplete#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
silent! inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
    return neocomplete#close_popup() . "\<CR>"
endfunction
" <TAB>: completion.
silent! inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
silent! inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
silent! inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
silent! inoremap <expr><C-y>  neocomplete#close_popup()
silent! inoremap <expr><C-e>  neocomplete#cancel_popup()

" Auto close top window preview
silent! let g:neocomplete#enable_auto_close_preview=1
" Disable window preview
set completeopt-=preview

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim-go - Golang support
" git clone https://github.com/fatih/vim-go.git ~/.vim/bundle/vim-go
"
" Check https://github.com/fatih/vim-go
" Use :GoInstallBinaries to install all the needed binaries.
"
" Set gocode autobuild for updated autocompletion:
" $ gocode set autobuild true
"

" Shortcuts:
silent! au FileType go nmap <leader>r <Plug>(go-run)
silent! au FileType go nmap <leader>b <Plug>(go-build)
silent! au FileType go nmap <leader>t <Plug>(go-test)
silent! au FileType go nmap <leader>c <Plug>(go-coverage)
silent! au FileType go nmap <Leader>ds <Plug>(go-def-split)
silent! au FileType go nmap <Leader>dv <Plug>(go-def-vertical)
silent! au FileType go nmap <Leader>dt <Plug>(go-def-tab)
silent! au FileType go nmap <Leader>gd <Plug>(go-doc)
silent! au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)
silent! au FileType go nmap <Leader>gb <Plug>(go-doc-browser)
silent! au FileType go nmap <Leader>s <Plug>(go-implements)
silent! au FileType go nmap <Leader>i <Plug>(go-info)
silent! au FileType go nmap <Leader>e <Plug>(go-rename)
" Emulate tags:
silent! au FileType go nmap <C-]> <Plug>(go-def)
" Replace go run:
silent! au FileType go nmap <Leader>r :!go run %<CR>

" Highlights:
silent! let g:go_highlight_functions = 1
silent! let g:go_highlight_methods = 1
silent! let g:go_highlight_structs = 1
silent! let g:go_highlight_interfaces = 1
silent! let g:go_highlight_operators = 1
silent! let g:go_highlight_build_constraints = 1

" Go fmt with organize imports:
silent! let g:go_fmt_command = "goimports"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Nerd tree - Browse files
" git clone https://github.com/scrooloose/nerdtree.git ~/.vim/bundle/nerdtree

" Shortcut:
silent! nmap <Leader>; :NERDTreeToggle<CR>
silent! au FileType nerdtree nmap <buffer> <left> x
silent! au FileType nerdtree nmap <buffer> <right> o
" Close vim when Nerd tree is the last window
silent! autocmd bufenter *
    \ if (winnr("$") == 1 
    \ && exists("b:NERDTree")
    \ && b:NERDTree.isTabTree()) | q | endif
" General
silent! let NERDTreeDirArrows=1
silent! let NERDTreeMinimalUI=1
silent! let NERDTreeIgnore=['\.o$', '\.pyc$', '\.php\~$']
silent! let NERDTreeWinSize = 30
silent! let NERDTreeQuitOnOpen=1
silent! let NERDTreeCascadeOpenSingleChildDir=1
