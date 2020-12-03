set nocompatible

set ignorecase
set smartcase

inoremap jk <esc>
set clipboard=unnamed

" Search as you type, highlight results
set incsearch
set showmatch
set hlsearch
nmap <esc> :nohlsearch<cr>

set encoding=utf-8
set tabstop=8
set shiftwidth=8
set autoindent
set magic

set number
set scrolloff=3
set sidescroll=3

set ruler
set cc=80
set tw=80
set nowrap

set splitbelow
set splitright
set hidden
set notimeout

set t_Co=256

nmap <backspace> %

nmap <C-s> :w<cr>
let mapleader = ","

set termguicolors
colorscheme desert

" Golang
autocmd FileType go map <Leader>r :w<cr>:!go build && ./$(basename $PWD)<cr>
autocmd FileType go map <Leader>t :w<cr>:!go test ./...<cr>
autocmd FileType go map <Leader>f :w<cr>:!gofmt -w %<cr>:e! %<cr>
autocmd FileType go map <Leader>b :w<cr>:!go build<cr>

call plug#begin('~/.config/nvim/autoload/plugged')
    Plug 'sheerun/vim-polyglot'
    Plug 'scrooloose/NERDTree'
    Plug 'jiangmiao/auto-pairs'
    Plug 'fiatjaf/neuron.vim'
call plug#end()
