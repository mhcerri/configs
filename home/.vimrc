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

set nocompatible

" Don't use the default leader key
let mapleader=";"

" Initialize Pathogen
silent! execute pathogen#infect()

" Jump to the last position when reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
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
set title
set hidden

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

" Syntax, color and highlight
filetype plugin indent on
syntax on
set number
set hlsearch 
try
    " Inverse priority list of preferred color schemes:
    colorscheme industry
    hi ColorColumn ctermbg=232 ctermfg=1
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
    " Auto-pair - Include or remove matching bracket and parenthesis
    " git clone git://github.com/jiangmiao/auto-pairs.git ~/.vim/bundle/auto-pairs

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " CsApprox - Better support with colorschemes
    " git clone https://github.com/godlygeek/csapprox.git ~/.vim/bundle/csapprox

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Jedi-vim - Python support
    " git clone --recursive https://github.com/davidhalter/jedi-vim.git ~/.vim/bundle/jedi-vim

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Vim bufferline - List buffer in the command line
    " git clone https://github.com/bling/vim-bufferline ~/.vim/bundle/vim-bufferline
    if exists("g:bufferline_echo")
        let g:bufferline_echo = 1
        let g:bufferline_fixed_index = 0
        let g:bufferline_rotate = 1
        let g:bufferline_show_bufnr = 1
        let g:bufferline_active_buffer_left = '['
        let g:bufferline_active_buffer_right = ']'
    endif

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
        " Cycle through errors
        nnoremap <F2> :try<CR>lnext<CR>catch /E42/<CR>catch /E553/<CR>lfirst<CR>endtry<CR><CR>
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
        inoremap <expr><C-g>     neocomplete#undo_completion()
        inoremap <expr><C-l>     neocomplete#complete_common_string()

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
        au FileType go nmap <leader>r <Plug>(go-run)
        au FileType go nmap <leader>b <Plug>(go-build)
        au FileType go nmap <leader>t <Plug>(go-test)
        au FileType go nmap <leader>c <Plug>(go-coverage)
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
        " Replace go run:
        au FileType go nmap <Leader>r :!go run %<CR>

        " Highlights:
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
