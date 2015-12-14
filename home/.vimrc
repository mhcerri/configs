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

autocmd FileType python,html,htmldjango,css,javascript,scala set
    \ tabstop=2
    \ softtabstop=2
    \ shiftwidth=2

autocmd Filetype make set
    \ noexpandtab

" Syntax, color and highlight
syntax on
set number
set hlsearch 
try
    colorscheme desert
    colorscheme github
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
    \ ctermbg = none
    \ ctermfg = none
    \ guibg   = none
    \ guifg   = none

" Reopen a file at the same position
" State is saved in ~/.vim/view/
au BufWinLeave *.* mkview!
au BufWinEnter *.* silent loadview

" Search tag files in parent directories
set tags=tags;

" Buffer shortcuts
:nnoremap <Tab> :bnext<CR>
:nnoremap <S-Tab> :bprevious<CR>

" Tabs shortcuts
:nnoremap . :tabnext<CR>
:nnoremap , :tabprevious<CR>
:nnoremap > :tabmove +1<CR>
:nnoremap < :tabmove -1<CR>

" Snippets
au BufNewFile,BufRead *.md iabbrev ''' ```
