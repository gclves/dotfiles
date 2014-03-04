set nocompatible
filetype off
filetype plugin indent on
set encoding=utf-8

" Vundle config
source ~/.vimrc.bundle

if has("gui_running")
    set guioptions=
    if has("win32")
        au GUIEnter * simalt ~x
    else
        set lines=999 columns=999
        set guifont=Inconsolata 10
    endif
else
    if $COLORTERM == "gnome-terminal"
        set t_Co=256
        set columns=80 lines=100
    endif
    if $TERM == "screen-256color"
        set t_Co=256
        set columns=80 lines=100
    endif
endif

set background=dark
colorscheme solarized

let mapleader=","
let g:airline_powerline_fonts=1

" mixed numbering
set relativenumber
set number
"we don't need relative numbers when we're out of focus
autocmd FocusLost * :set norelativenumber
autocmd FocusGained * :set relativenumber
" shows absolute numbers on insert mode
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

"move around panes
nnoremap <C-H> <C-w>h
nnoremap <C-J> <C-w>j
nnoremap <C-K> <C-w>k
nnoremap <C-L> <C-w>l

" move around buffers
nnoremap <F5> :buffers<CR>:buffer<Space>
nnoremap <C-Tab> :b#<CR>

" quick save
nnoremap <C-s> :w<CR>
inoremap <C-s> <Esc>:w<CR>a

" move around in I-mode
inoremap <C-h> <Left>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Right>

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
nnoremap <Esc> :nohl<CR>

" Wildmode
set wildchar=<Tab> wildmenu wildmode=full

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
nnoremap j gj
nnoremap k gk

" Indent
set autoindent
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4

" Node.js
autocmd FileType javascript set dictionary+=~/.vim/bundle/vim-node/dict/node.dict

" Javascript
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2

" Startify
nnoremap <F8> :Startify<CR>

" NERDtree
nnoremap <C-e> :NERDTreeToggle<CR>

" CtrlP options
nnoremap ; :CtrlPMixed<CR>
let g:ctrlp_custom_ignore = '\v\~$|\.(o|swp|pyc|wav|mp3|ogg|blend)$|(^|[/\\])\.(hg|git|bzr)($|[/\\])|__init__\.py'

unlet g:ctrlp_custom_ignore
let g:ctrlp_custom_ignore = {
                            \ 'file': '\.(o|swp|pyc|wav|mp3|ogg|blend|exe|so)$',
                            \ 'dir' : '\.(hg|git|bzr)$'
                            \ }

" Syntastic options
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=1

let g:syntastic_javascript_checkers=['jshint', 'jslint']
let g:syntastic_html_checkers=['jshint']
let g:syntastic_jshint_exec='jshint.cmd'
let g:syntastic_javascript_jshint_conf='~/.jshintrc'
