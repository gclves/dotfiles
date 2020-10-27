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

nmap <C-s> :w<cr>

set termguicolors
colorscheme desert

call plug#begin('~/.config/nvim/autoload/plugged')
    Plug 'sheerun/vim-polyglot'
    Plug 'scrooloose/NERDTree'
    Plug 'jiangmiao/auto-pairs'
    Plug 'fiatjaf/neuron.vim'
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
call plug#end()
