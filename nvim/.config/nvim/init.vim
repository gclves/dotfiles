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

" fzf config
map <Leader>. :Files<cr>
if exists('$TMUX')
  let g:fzf_layout = { 'tmux': '-p90%,60%' }
else
  let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6 } }
endif
autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

set background=dark
set termguicolors

set relativenumber

" FIXME the below should be different
" We should unconditionally set <Leader>r to run some `run` cmd
" `run` should then check the FileType and dispatch to runners accordingly

" Golang
autocmd FileType go map <Leader>r :w<cr>:!go build && ./$(basename $PWD)<cr>
autocmd FileType go map <Leader>t :w<cr>:!go test ./...<cr>
autocmd FileType go map <Leader>f :w<cr>:!gofmt -w %<cr>:e! %<cr>
autocmd FileType go map <Leader>b :w<cr>:!go build<cr>

" Ruby
augroup rubypath
    autocmd!

    autocmd FileType ruby setlocal suffixesadd+=.rb
augroup END

call plug#begin('~/.config/nvim/autoload/plugged')
    Plug 'sheerun/vim-polyglot'
    Plug 'scrooloose/NERDTree'
    Plug 'jiangmiao/auto-pairs'
    Plug 'fiatjaf/neuron.vim'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-bundler'
    Plug 'tpope/vim-rails'
    Plug 'airblade/vim-gitgutter'
    Plug 'wellle/targets.vim'
    Plug 'vim-ruby/vim-ruby'
    Plug 'junegunn/fzf'
    Plug 'junegunn/fzf.vim'
call plug#end()

if has('nvim')
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1

  " Fix vim-tmux-navigator <C-h> https://git.io/viGRU
  nmap <BS> <C-W>h

  " Fix vim-tmux-navigator <C-h> https://git.io/vS5QH
  nmap <BS> :<C-u>TmuxNavigateLeft<CR>
endif

