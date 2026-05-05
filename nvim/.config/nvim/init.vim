" TODO:
" - run unit tests
" - TreeSitter?

" === General Settings ===
set nocompatible      " Use Vim defaults, not Vi compatibility
set encoding=utf-8    " Set default encoding to UTF-8
set fileencoding=utf-8 " File encoding for reading/writing files
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

" XXX: Experimental
Plug 'neovim/nvim-lspconfig'
Plug 'williamboman/mason.nvim'
Plug 'williamboman/mason-lspconfig.nvim'
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
        \ 'rust': 'rustc % -o %:r && ./%:r',
        \ 'elixir': 'mix run',
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

autocmd Filetype help nnoremap <buffer> q :q<CR>
autocmd FileType qf nnoremap <buffer> q :close<CR>

set background=dark
colorscheme default

" === EXPERIMENTAL ===

" Install vim-plug if not found
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

" Basic LSP setup
lua << EOF
local servers = {
    'lua_ls',
    'gopls',
    'rust_analyzer',
    'elixirls',
    'ts_ls'
}

require('mason').setup()
require('mason-lspconfig').setup({
  ensure_installed = servers
})

-- Basic LSP server configurations
local lspconfig = require('lspconfig')

for _, server in ipairs(servers) do
    lspconfig[server].setup({})
end
EOF

command! Format lua vim.lsp.buf.format({ async = false })

" LSP Keybindings
nnoremap <Leader>rn :lua vim.lsp.buf.rename()<CR>
nnoremap gr :lua vim.lsp.buf.references()<CR>

" LSP keybindings that fall back to Vim's native functionality
function! HasLsp()
    return luaeval('#vim.lsp.get_clients({ bufnr = 0 }) > 0')
endfunction

function! GoToDefinition()
    if has('nvim') && HasLsp()
        lua vim.lsp.buf.definition()
    else
        normal! gd
    endif
endfunction

function! ShowDocs()
    if has('nvim') && HasLsp()
        lua vim.lsp.buf.hover()
    else
        normal! K
    endif
endfunction

nnoremap gd :call GoToDefinition()<CR>
nnoremap K :call ShowDocs()<CR>

" Format on save
function! CleanWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfunction

function! FormatBeforeSave()
    " I'm slowly moving out of LSP for formatting. This stuff is complex and
    " frustrating. go doesn't need this madness
    if &filetype !=# "go"
        Format
    endif
endfunction

" Go: use the explicit CLI tool for formatting/imports.
function! GoImports()
    let l:save = winsaveview()
    silent! execute '%!goimports'
    call winrestview(l:save)
endfunction

augroup formatting
    autocmd!
    autocmd BufWritePre * call CleanWhitespace()
    autocmd BufWritePre * call FormatBeforeSave()
    autocmd BufWritePre *.go call GoImports()
augroup END

" Diagnostics
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focus = false })

nnoremap <Leader>d :lua vim.diagnostic.open_float()<CR>
nnoremap <Leader>dl :lua vim.diagnostic.setloclist()<CR>
nnoremap <Leader>dq :lua vim.diagnostic.setqflist()<CR>

" Search
command! -nargs=+ Grep silent grep! <args> | copen
nnoremap <Leader>/ :Grep<Space>
nnoremap <Leader>* :Grep <C-r><C-w><CR>

" Autocomplete
function! CompleteOrLsp()
    if pumvisible()
        return "\<C-n>"
    endif

    if has('nvim') && HasLsp()
        return "\<C-x>\<C-o>"
    endif

    return "\<C-n>"
endfunction

inoremap <expr> <C-n> CompleteOrLsp()

" === Running tests ===
function! SetTestCommand(command)
    if !exists('g:project_test_commands')
        let g:project_test_commands = {}
    endif

    let g:project_test_commands[getcwd()] = a:command
endfunction

function! QuickfixHasFileEntries()
    for l:item in getqflist()
        if get(l:item, 'bufnr', 0) > 0 && get(l:item, 'lnum', 0) > 0
            return 1
        endif
    endfor

    return 0
endfunction

function! RunTests()
    if !exists('g:project_test_commands') ||
                \ !has_key(g:project_test_commands, getcwd())
        echo 'No test command set. Use :SetTestCommand ...'
        return
    endif

    write
    let l:old_makeprg = &makeprg
    let &makeprg = g:project_test_commands[getcwd()]

    try
        silent make!
        if QuickfixHasFileEntries()
            botright copen
            wincmd p
        else
            cclose
            echo 'Tests passed'
        endif
    finally
        let &makeprg = l:old_makeprg
    endtry
endfunction

command! -nargs=+ SetTestCommand call SetTestCommand(<q-args>)
nnoremap <Leader>t :call RunTests()<CR>
