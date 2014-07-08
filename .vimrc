set nocompatible
filetype off
filetype plugin indent on
set encoding=utf-8

if has("gui_running")
    set guioptions=
    if has("win32")
        au GUIEnter * simalt ~x
    else
        set lines=999 columns=999
        set guifont=Monaco\ 8
        "set guifont=Inconsolata 10
    endif
elseif $COLORTERM == "gnome-terminal"
    set t_Co=256
    set columns=80 lines=100
elseif $TERM == "screen-256color"
    set t_Co=256
endif

" Vundle config
source ~/.vimrc.bundle

set background=dark
colorscheme solarized

syntax on
let mapleader=","
"let g:airline_powerline_fonts=1

" mixed numbering
set relativenumber
set number
"we don't need relative numbers when we're out of focus
"autocmd FocusLost * :set norelativenumber
"autocmd FocusGained * :set relativenumber
" shows absolute numbers on insert mode
"autocmd InsertEnter * :set norelativenumber
"autocmd InsertLeave * :set relativenumber

"move around panes
nnoremap <C-H> <C-w>h
nnoremap <C-J> <C-w>j
nnoremap <C-K> <C-w>k
nnoremap <C-L> <C-w>l

" move around buffers
nnoremap <C-Tab> :buffers<CR>:buffer<Space>
"nnoremap <C-Tab> :b#<CR>

" quickly get out of i-mode
inoremap jk <Esc>

" quick save
nnoremap <C-s> :w<CR>
inoremap <C-s> <Esc>:w<CR>a

" move around in I-mode
"inoremap <C-h> <Left>
"inoremap <C-j> <Down>
"inoremap <C-k> <Up>
"inoremap <C-l> <Right>

" % is too far out to reach
map <BS> %

" Status line
set laststatus=2
set statusline=%<%f\				" filename
set statusline+=%w%h%m%r			" options
set statusline+=%{fugitive#statusline()}	" git
set statusline+=\ [%{&ff}/%Y]			" filetype
set statusline+=\ [%{getcwd()}]			" directory
set statusline+=%=%-14.(%l,%c%V%)\ %p%%		" file info

set showmatch		" show matching brackets/parenthesis
set matchtime=0		" don't blink
set incsearch		" find as you type
" Highlight search terms
set hlsearch
nnoremap <Esc><Esc> :nohlsearch<CR>

" Wildmode
set wildchar=<Tab> wildmenu wildmode=full

set cursorline
set winminheight=0	" windows can be 0-line high
set ignorecase		" case-insensitive search...
set smartcase		" ...unless it's uppercase
set clipboard=unnamed	" use the OS clipboard
" improve windows/unix compatibility
set viewoptions=folds,options,cursor,unix,slash
set history=100		" store more command history
set undolevels=400	" store more undo history
set spell		" spellchecking
set hidden		" buffer switching without saving

" Fix weird behavior on wrapped lines
map j gj
map k gk

" Indent
set autoindent
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4

set tw=79
" "hard" word wrap
set formatoptions+=t
" "soft" word wrap
"set wrap linebreak nolist

au BufRead,BufNewFile *.txt,*.tex,*.md,*.markdown set wrap linebreak nolist textwidth=0 wrapmargin=0

" Node.js
autocmd FileType javascript set dictionary+=~/.vim/bundle/vim-node-dict/dict/node.dict

" Javascript
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2

" NERDtree
nnoremap <C-e> :NERDTreeToggle<CR>

" CtrlP options
"map ; :CtrlPMixed<CR>

let g:ctrlp_custom_ignore = {
                            \ 'file': '\.(o|swp|pyc|wav|mp3|ogg|blend|exe|so)$',
                            \ 'dir' : '\.(hg|git|bzr)$'
                            \ }

" Ag + CtrlSF
nnoremap <C-O> :CtrlSF<Space>

" Syntastic options
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=1

let g:syntastic_javascript_checkers=['jshint', 'jslint']
let g:syntastic_html_checkers=['jshint']
let g:syntastic_jshint_exec='jshint.cmd'
let g:syntastic_javascript_jshint_conf='~/.jshintrc'

" Tagbar
nnoremap <silent> <Leader>b :TagbarToggle<CR>
nnoremap <Leader>. :CtrlPTag<CR>

" a better encryption algorithm
set cryptmethod=blowfish

" this is where the coding happens
"cd ~/www
