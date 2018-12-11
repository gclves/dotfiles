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

" Work-around incomplete terminfo databases
" Particulalry useful when under `screen`, which may or may not be attached to
" a physical terminal capable of 256color mode
if match($TERMCAP, 'Co#256:') == 0 || match($TERMCAP, ':Co#256:') > 0
    set t_Co=256
endif

" Vundle config
source ~/.vimrc.bundle

set background=light
colorscheme Tomorrow

syntax on
let mapleader=","
"nnoremap ; :
let g:airline_powerline_fonts=1

" mixed numbering
set relativenumber
set number

"move around panes
nnoremap <C-H> <C-w>h
nnoremap <C-J> <C-w>j
nnoremap <C-K> <C-w>k
nnoremap <C-L> <C-w>l

" move around buffers
"nnoremap <C-Tab> :buffers<CR>:buffer<Space>
"nnoremap <C-Tab> :b#<CR>

" quickly get out of i-mode with jk
inoremap jk <Esc>

" quick save
nnoremap <C-s> :w<CR>
inoremap <C-s> <Esc>:w<CR>a
" Force save when I forget to sudo
cnoremap w!! w !sudo tee % >/dev/null

" move around in I-mode
inoremap <C-h> <Left>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Right>

" Use Backspace for matching brackets
map <BS> %

" sometimes I end up using tabs
nnoremap <C-t> :tabnew<CR>
nnoremap <C-Tab> :tabNext<CR>
nnoremap <C-W>w :tabclose<CR>

" No backups (file~) please
set nobackup
set nowritebackup

" Status line
set laststatus=2
set statusline=%<%f\				        " filename
set statusline+=%w%h%m%r			        " options
set statusline+=%{fugitive#statusline()}	" git
set statusline+=\ [%{&ff}/%Y]			    " filetype
set statusline+=\ [%{getcwd()}]			    " directory
set statusline+=%=%-14.(%l,%c%V%)\ %p%%		" file info

set showmatch		" show matching brackets/parenthesis
set matchtime=0		" don't blink
set incsearch		" find as you type
" Highlight search terms
set hlsearch
nnoremap <Esc><Esc> :nohlsearch<CR>

" Wildmode
set wildchar=<Tab> wildmenu wildmode=full

set cursorline          " highlight selected line
set winminheight=0	    " windows can be 0-line high
set ignorecase		    " case-insensitive search...
set smartcase		    " ...unless it's uppercase
set clipboard=unnamed	" use the OS clipboard
" improve windows/unix compatibility
set viewoptions=folds,options,cursor,unix,slash
set history=100		    " store more command history
set undolevels=400	    " store more undo history
set spell		        " spellchecking
set hidden		        " buffer switching without saving
" highlight columns 80 and 120 so I know when to wrap
let &colorcolumn="80,".join(range(120,999),",")

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

au BufRead,BufNewFile *.less set filetype=less
au BufRead,BufNewFile *.php set nocursorline | syntax sync minlines=100 | syntax sync maxlines=240 | set synmaxcol=800

set foldmethod=indent
set foldlevel=8

" Native alternative to NERDtree
let g:netrw_liststyle = 3
nnoremap <C-e> :Explore<CR>

" nnoremap <C-e> :NERDTreeToggle<CR>
" autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | cd ~/www | NERDTree | endif

" CtrlP options
"map ; :CtrlPMixed<CR>

let g:ctrlp_custom_ignore = {
                            \ 'file': '\.(o|swp|pyc|wav|mp3|ogg|blend|exe|so)$',
                            \ 'dir' : '\.(hg|git|bzr|svn|data)$'
                            \ }
"let g:ctrlp_match_window_bottom = 0
"let g:ctrlp_match_window_reversed = 0

" Ag
map <Leader>f :CtrlSF --ignore data/<Space>
map <Leader>d :CtrlSFOpen<CR>

" Syntastic options
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=1

let g:syntastic_javascript_checkers=['jshint']
let g:syntastic_html_checkers=['jshint']
"let g:syntastic_php_checkers = ['phpcs', 'php', 'phpmd']
"let g:syntastic_javascript_jshint_conf='~/.jshintrc'

" Vim Javascript
let javascript_enable_domhtmlcss = 1
let b:javascript_fold = 1
let g:javascript_conceal = 1
let g:used_javascript_libs = 'underscore,backbone,jquery'

" Tagbar
nnoremap <silent> <Leader>b :TagbarToggle<CR>
map <F7> :TagbarToggle<CR>
"nnoremap <Leader>. :CtrlPTag<CR>

" Easymotion
map <Leader> <Plug>(easymotion-prefix)

" Dynamic search with easymotion
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)

" Alt+<hjkl> to move around long distances
map <M-l> <Plug>(easymotion-lineforward)
map <M-j> <Plug>(easymotion-j)
map <M-k> <Plug>(easymotion-k)
map <M-h> <Plug>(easymotion-linebackward)

let g:EasyMotion_startofline = 0 " keep cursor colum when JK motion

" Emmet
let g:user_emmet_leader_key='<Leader>.'

" Go straight to ~/Code if no directory has been specified
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | cd ~/Code

" a better encryption algorithm
set cryptmethod=blowfish
