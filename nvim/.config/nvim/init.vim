" === General Settings ===
set nocompatible      " Use Vim defaults, not Vi compatibility
set encoding=utf-8    " Set default encoding to UTF-8
set fileencoding=utf-8 " File encoding for reading/writing files
set termencoding=utf-8 " Encoding for terminal communication
set hidden             " Allow switching buffers without saving
set history=10000       " Increase history size for commands and searches
set undofile           " Enable persistent undo
set nobackup           " Disable backup files
set noswapfile         " Disable swap files (use persistent undo instead)
set showcmd            " Display incomplete commands
set wildmenu           " Enhanced command line completion
set ignorecase        " Ignore case when searching
set smartcase          " Override 'ignorecase' if search contains uppercase
set incsearch          " Incremental search (live feedback)
set hlsearch           " Highlight search results
set scrolloff=5        " Keep 5 lines above/below cursor when scrolling
set number             " Show line numbers
set relativenumber     " Show relative line numbers
set cursorline         " Highlight current line
set wrap               " Wrap long lines
set linebreak          " Break lines at word boundaries
set tabstop=4          " Set tab width to 4 spaces
set softtabstop=4      " Set number of spaces inserted for <Tab>
set shiftwidth=4       " Indentation level for < and > commands
set expandtab          " Use spaces instead of tabs
set smarttab
set autoindent         " Enable auto indentation
set smartindent        " Smarter auto indentation based on file type
set cindent            " C-style indentation (if applicable)
set ruler
set showmatch
set laststatus=2
set lazyredraw
filetype plugin indent on " Load filetype-specific plugins and indenting
syntax on               " Enable syntax highlighting
colorscheme desert      " Set your preferred colorscheme
let mapleader=","

" === Navigation & Editing ===
set backspace=indent,eol,start " Backspace over everything in insert mode
set whichwrap+=<,>,[,] " Allow cursor movement between lines with <, >, [, ]
" Move to window below with Ctrl+j
map <C-j> <C-W>j
" Move to window above with Ctrl+k
map <C-k> <C-W>k
" Exit insert mode with 'jk'
inoremap jk <Esc>
inoremap kj <Esc>

nmap k gk
map j gj

" Navigate errors
map <C-n> :cn<cr>
map <C-p> :cp<cr>

" === Search & Replace ===
" Use 'very magic' mode for search by default
nnoremap / /\v
nnoremap ? ?\v
cnoremap %s/ %smagic/
cnoremap \>s/ \>smagic/
nnoremap :g/ :g/\v
nnoremap :g// :g//

" === Splits & Tabs ===
" Move to left split with Ctrl+h
nnoremap <C-h> <C-w>h
" Move to right split with Ctrl+l
nnoremap <C-l> <C-w>l
" Move to split below with Ctrl+j
nnoremap <C-j> <C-w>j
" Move to split above with Ctrl+k
nnoremap <C-k> <C-w>k

" === Plugins ===
" Add your plugin management system (e.g., vim-plug, Vundle) and plugins here
" ...

" === Custom Mappings and Functions ===
" Dictionary to map file types to run commands
let g:run_commands = {
    \ 'vim': 'source %',
    \ 'go': '!go run %',
    \ 'py': '!python %', 
    \ 'rb': '!ruby %',
    \ 'js': '!node %'
    \ }

nnoremap <Leader>r :w<cr>:execute get(g:run_commands, expand('%:e'), 'echoerr "No command defined for this file type"')<CR>

" swapping : and ; save a lot of unneeded shifting:
noremap ; :
noremap : ;

" edit vimrc with ,ev
nnoremap <Leader>ev :tabe $MYVIMRC<cr>

" <C-s> to save
inoremap <C-s> <Esc>:w<cr>a
nnoremap <C-s> :w<cr>

imap <C-t> <esc>:tabnew<cr>

autocmd Filetype help nmap <buffer> q :q<CR>
