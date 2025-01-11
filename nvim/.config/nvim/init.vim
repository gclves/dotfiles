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
set clipboard+=unnamedplus  " use the system clipboard
filetype plugin indent on " Load filetype-specific plugins and indenting
syntax on               " Enable syntax highlighting
let mapleader="\<Space>"

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
noremap <C-n> :cn<cr>
noremap <C-p> :cp<cr>

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
call plug#begin()

" List your plugins here
Plug 'tpope/vim-sensible'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'elixir-editors/vim-elixir'

call plug#end()

" === Run commands ==
" Extended version with project-specific settings
function! SetProjectRunCommand(filetype, command)
    " Get the project root directory (this is a simple version)
    let l:project_root = getcwd()

    " Initialize the project commands dictionary if it doesn't exist
    if !exists('g:project_run_commands')
        let g:project_run_commands = {}
    endif

    " Initialize the nested dictionary if needed
    if !has_key(g:project_run_commands, l:project_root)
        let g:project_run_commands[l:project_root] = {}
    endif

    " Set the command for this filetype in this project
    let g:project_run_commands[l:project_root][a:filetype] = a:command
endfunction

function! RunFile()
    if !filereadable(expand('%'))
        echo "Error: File not saved"
        return
    endif

    " Default commands
    let l:default_commands = {
        \ 'python': 'python3 %',
        \ 'ruby': 'ruby %',
        \ 'go': 'go run %',
        \ 'javascript': 'node %',
        \ 'typescript': 'ts-node %',
        \ 'sh': 'sh %',
        \ 'c': 'gcc % -o %:r && ./%:r',
        \ 'rust': 'rustc % -o %:r && ./%:r'
        \ }

    " Get project root
    let l:project_root = getcwd()

    " Check for project-specific override first
    let l:cmd = ''
    if exists('g:project_run_commands') &&
        \ has_key(g:project_run_commands, l:project_root) &&
        \ has_key(g:project_run_commands[l:project_root], &filetype)
        let l:cmd = g:project_run_commands[l:project_root][&filetype]
    else
        " Fall back to global override, then default
        let l:cmd = get(l:default_commands, &filetype, '')
    endif

    if l:cmd == ''
        echo "No run configuration for filetype: " . &filetype
        return
    endif

    " Save the file first
    write
    " Store the current directory
    let l:cur_dir = getcwd()

    try
        lcd %:p:h
        silent !clear
        execute '!' . l:cmd
    finally
        " Always change back to the original directory, even if there's been
        " an error
        execute 'lcd ' . l:cur_dir
    endtry
endfunction

" Add commands for both global and project-specific settings
command! -nargs=+ SetProjectRunCommand call SetProjectRunCommand(<f-args>)
nnoremap <Leader>r :call RunFile()<cr>

" === Whitespace cleanup ===
function! CleanWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfunction

autocmd BufWritePre * call CleanWhitespace()

" === Custom Mappings and Functions ===
noremap <Escape> :nohlsearch<cr>

" swapping : and ; save a lot of unneeded shifting:
noremap ; :
noremap : ;

" edit vimrc with ,ev
nnoremap <Leader>ev :tabe $MYVIMRC<cr>

" fzf
nnoremap <Leader>f :Files<cr>
noremap <Leader>b :Buffers<cr>

" tests

" <C-s> to save
inoremap <C-s> <Esc>:w<cr>a
noremap <C-s> :w<cr>

inoremap <C-t> <esc>:tabnew<cr>

autocmd Filetype help nmap <buffer> q :q<CR>

set background=light
colorscheme quiet
