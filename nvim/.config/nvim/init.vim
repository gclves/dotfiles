set nocompatible

call plug#begin('~/.config/nvim/autoload/plugged')
    Plug 'sheerun/vim-polyglot'
    Plug 'scrooloose/NERDTree'
    Plug 'jiangmiao/auto-pairs'
    Plug 'fiatjaf/neuron.vim'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-bundler'
    Plug 'tpope/vim-rails'
    Plug 'tpope/vim-rbenv'
    Plug 'airblade/vim-gitgutter'
    Plug 'wellle/targets.vim'
    Plug 'vim-ruby/vim-ruby'
    Plug 'junegunn/fzf'
    Plug 'junegunn/fzf.vim'
    Plug 'preservim/vimux'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'pyrho/nerveux.nvim'
    Plug 'thoughtbot/vim-rspec'
    Plug 'tpope/vim-dispatch'

    " Optional but recommended for better markdown syntax
    Plug 'plasticboy/vim-markdown'
call plug#end()

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
map <Leader>b :w<cr>:make<cr>

command JsonPrettyPrint %!jq .

" fzf config
map <Leader>. :Files<cr>
map <Leader>F :Rg<cr>
map <Leader>^ :Buffers<cr>

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

" Golang
augroup gopath
    autocmd!

    autocmd FileType go setlocal suffixesadd+=.go

    autocmd FileType go nnoremap <buffer> <Leader>r :w<cr>:!go build && ./$(basename $PWD)<cr>
    autocmd FileType go nnoremap <buffer> <Leader>t :w<cr>:!go test ./...<cr>
    autocmd FileType go nnoremap <buffer> <Leader>f :w<cr>:!goimports -w %<cr>:e! %<cr>

    autocmd FileType go 
      \ compiler go | setl makeprg=go\ build
augroup END

" Ruby
" command RSpec !bundle exec rspec %
augroup rubypath
    autocmd!

    autocmd FileType ruby setlocal suffixesadd+=.rb
    autocmd FileType ruby nnoremap <buffer> <Leader>r :w<cr>:!ruby %<cr>
    autocmd FileType ruby
      \ if expand("%") =~# '_spec\.rb$' |
      \   compiler rspec | setl makeprg=bundle\ exec\ rspec\ $*
      \ else |
      \   compiler ruby | setl makeprg=ruby\ -wc\ \"%:p\" 
      \ endif

    "let g:rspec_command = "Dispatch rspec {spec}"

    " RSpec.vim mappings
    map <Leader>t :call RunCurrentSpecFile()<CR>
    map <Leader>s :call RunNearestSpec()<CR>
    map <Leader>l :call RunLastSpec()<CR>
    map <Leader>a :call RunAllSpecs()<CR>
augroup END

augroup pythonpath
	autocmd!

	autocmd FileType python setlocal suffixesadd+=.py
	autocmd FileType python nnoremap <buffer> <Leader>r :w<cr>:!python %<cr>
augroup END

" Terminal
tnoremap <Esc> <C-\><C-n>
tnoremap <A-[> <Esc>
tnoremap <a-h> <c-\><c-n><c-w>h
tnoremap <a-j> <c-\><c-n><c-w>j
tnoremap <a-k> <c-\><c-n><c-w>k
tnoremap <a-l> <c-\><c-n><c-w>l
" No line numbers on terminals
autocmd TermOpen * setlocal nonumber norelativenumber

autocmd FileType javascript setlocal sw=2 sta
autocmd FileType typescript setlocal sw=2 sta

" Vim
autocmd FileType vim nnoremap <buffer> <Leader>b :w<cr>:source %<cr>

if has('nvim')
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1

  " Fix vim-tmux-navigator <C-h> https://git.io/viGRU
  nmap <BS> <C-W>h

  " Fix vim-tmux-navigator <C-h> https://git.io/vS5QH
  nmap <BS> :<C-u>TmuxNavigateLeft<CR>
endif

map <Leader>vp :VimuxPromptCommand<cr>
map <Leader>vl :VimuxRunLastCommand<cr>

