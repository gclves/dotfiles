" TODO:
" - TreeSitter? Maybe?

" === General Settings ===
set nocompatible      " Use Vim defaults, not Vi compatibility
set encoding=utf-8    " Set default encoding to UTF-8
set fileencoding=utf-8 " File encoding for reading/writing files
set hidden             " Allow switching buffers without saving
set history=10000       " Increase history size for commands and searches
set undofile           " Enable persistent undo
set nobackup           " Disable backup files

" Create the {swap, undo} folders if they don't exist
if !isdirectory(expand('~/.local/state/nvim/swap'))
  call mkdir(expand('~/.local/state/nvim/swap'), 'p')
endif

if !isdirectory(expand('~/.local/state/nvim/undo'))
  call mkdir(expand('~/.local/state/nvim/undo'), 'p')
endif
set backupdir=~/.local/state/nvim/backup//
set directory=~/.local/state/nvim/swap//
set undodir=~/.local/state/nvim/undo//

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
set ruler
set showmatch
set laststatus=2
set lazyredraw
set switchbuf=useopen,usetab,newtab
set shortmess+=I       " Disable the intro message
set clipboard+=unnamedplus  " use the system clipboard
filetype plugin indent on " Load filetype-specific plugins and indenting
syntax on               " Enable syntax highlighting
set inccommand=split
set termguicolors
let mapleader="\<Space>"

set grepprg=rg\ --vimgrep

" === Navigation & Editing ===
set backspace=indent,eol,start " Backspace over everything in insert mode
set whichwrap+=<,>,[,] " Allow cursor movement between lines with <, >, [, ]
" Exit insert mode with 'jk'
inoremap jk <Esc>
inoremap kj <Esc>

" Navigate errors
noremap <C-n> :cn<cr>
noremap <C-p> :cp<cr>

" Fast buffer movement
nnoremap <S-l> :bnext<CR>
nnoremap <S-h> :bprevious<CR>
nnoremap <leader><Tab> :b#<CR>
nnoremap <C-S-w> :bdelete<CR>

" Tabs as layouts/workspaces
nnoremap <leader>tn :tabnew<CR>
nnoremap <leader>tk :tabclose<CR>
nnoremap <leader>to :tabonly<CR>
nnoremap <leader>tl :tabnext<CR>
nnoremap <leader>th :tabprevious<CR>
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt

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

" Install vim-plug if not found
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()

" List your plugins here
Plug 'tpope/vim-sensible'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'elixir-editors/vim-elixir'
Plug 'sderev/alabaster.vim'

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
nnoremap <silent> <Esc> :nohlsearch<CR><Esc>

" swapping : and ; save a lot of unneeded shifting:
noremap ; :
noremap : ;

" edit vimrc with ,ev
nnoremap <Leader>ev :tabe $MYVIMRC<cr>

" fzf
nnoremap <Leader>f :GFiles<cr>
nnoremap <Leader>F :Files<cr>
noremap <Leader>b :Buffers<cr>

" <C-s> to save
inoremap <C-s> <Esc>:w<cr>a
noremap <C-s> :w<cr>

inoremap <C-t> <esc>:tabnew<cr>

autocmd Filetype help nnoremap <buffer> q :q<CR>
autocmd FileType qf nnoremap <buffer> q :close<CR>

set background=light
" Available variants: alabaster-bg, alabaster-dark, alabaster-mono, alabaster-dark-mono
colorscheme alabaster-dark

" === EXPERIMENTAL ===

" Basic LSP setup
lua << EOF
local servers = {
    'lua_ls',
    'gopls',
    'rust_analyzer',
    'elixirls',
    'ts_ls',
}

require('mason').setup()
require('mason-lspconfig').setup({
  ensure_installed = servers
})

function _G.lsp_format()
    local clients = vim.lsp.get_clients({
        bufnr = 0,
        method = 'textDocument/formatting',
    })

    if #clients == 0 then
        return false
    end

    vim.lsp.buf.format({
        async = false,
        bufnr = 0,
    })
    return true
end

-- Basic LSP server configurations
for _, server in ipairs(servers) do
    vim.lsp.config(server, {})
    vim.lsp.enable(server)
end

-- Diagnostic lines
vim.diagnostic.config({ virtual_lines = true })
EOF

command! Format lua lsp_format()

" LSP Keybindings
nnoremap <Leader>rn :lua vim.lsp.buf.rename()<CR>
nnoremap gr :lua vim.lsp.buf.references()<CR>

" LSP keybindings that fall back to Vim's native functionality
function! HasLsp()
    return luaeval('#vim.lsp.get_clients({ bufnr = 0 }) > 0')
endfunction

function! HasLspFormatter()
    return luaeval('#vim.lsp.get_clients({ bufnr = 0, method = "textDocument/formatting" }) > 0')
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
    if !HasLsp()
        let l:save = winsaveview()
        keeppatterns %s/\s\+$//e
        call winrestview(l:save)
    endif
endfunction

function! FormatBeforeSave()
    " I'm slowly moving out of LSP for formatting. This stuff is complex and
    " frustrating. go doesn't need this madness
    if &filetype !=# "go" && &filetype !=# "typescript" && HasLspFormatter()
        silent Format
    endif
endfunction

" Go: use the explicit CLI tool for formatting/imports.
function! GoImports()
    let l:save = winsaveview()
    silent! execute '%!goimports'
    call winrestview(l:save)
endfunction

" JS/TS: use prettier
function! PrettierFormat()
    let l:save = winsaveview()
    silent! execute '%!prettier --parser typescript'
    call winrestview(l:save)
endfunction

augroup formatting
    autocmd!
    autocmd BufWritePre * call CleanWhitespace()
    autocmd BufWritePre * call FormatBeforeSave()
    autocmd BufWritePre *.go call GoImports()
    autocmd BufWritePre *.js{x,} call PrettierFormat()
    autocmd BufWritePre *.ts{x,} call PrettierFormat()
augroup END

augroup reload
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END

" Diagnostics
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focus = false })

nnoremap <Leader>d :lua vim.diagnostic.open_float()<CR>
nnoremap <Leader>dl :lua vim.diagnostic.setloclist()<CR>
nnoremap <Leader>dq :lua vim.diagnostic.setqflist()<CR>

" Search
command! -nargs=+ Grep silent grep! <args> | copen
"nnoremap <Leader>/ :Grep<Space>
nnoremap <Leader>/ :Rg<cr>
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

function! DefaultTestCommand()
    let l:defaults = {
        \ 'go': 'go test ./...',
        \ 'elixir': 'mix test',
        \ 'ruby': 'bundle exec rspec',
        \ 'javascript': 'npm test',
        \ 'typescript': 'npm test',
        \ }

    return get(l:defaults, &filetype, '')
endfunction

function! GetTestCommand()
    if !exists('g:project_test_commands')
        let g:project_test_commands = {}
    endif

    let l:project_root = getcwd()
    if has_key(g:project_test_commands, l:project_root)
        return g:project_test_commands[l:project_root]
    endif

    let l:default = $NVIM_TEST_COMMAND !=# ''
        \ ? $NVIM_TEST_COMMAND
        \ : DefaultTestCommand()
    let l:cmd = input('Test command: ', l:default)

    if l:cmd !=# ''
        let g:project_test_commands[l:project_root] = l:cmd
    endif

    return l:cmd
endfunction

function! SetFileTestCommand(command)
    if !exists('g:project_file_test_commands')
        let g:project_file_test_commands = {}
    endif

    let g:project_file_test_commands[getcwd()] = a:command
endfunction

function! CurrentFileLooksLikeTest()
    let l:file = expand('%:t')

    if &filetype ==# 'python'
        return l:file =~# '^test_.*\.py$' || l:file =~# '_test\.py$'
    endif

    if &filetype ==# 'ruby'
        return l:file =~# '_spec\.rb$'
            \ || l:file =~# '^test_.*\.rb$'
            \ || l:file =~# '_test\.rb$'
    endif

    if &filetype ==# 'javascript'
        return l:file =~# '\.test\.jsx\?$' || l:file =~# '\.spec\.jsx\?$'
    endif

    if &filetype ==# 'typescript'
        return l:file =~# '\.test\.tsx\?$' || l:file =~# '\.spec\.tsx\?$'
    endif

    return 0
endfunction

function! BuildTestCommand(template)
    let l:file = expand('%')

    if l:file ==# ''
        return ''
    endif

    let l:relative_file = fnamemodify(l:file, ':.')
    let l:absolute_file = fnamemodify(l:file, ':p')
    let l:dir = fnamemodify(l:relative_file, ':h')

    let l:cmd = a:template
    let l:cmd = substitute(l:cmd, '{file_abs}', shellescape(l:absolute_file), 'g')
    let l:cmd = substitute(l:cmd, '{file}', shellescape(l:relative_file), 'g')
    let l:cmd = substitute(l:cmd, '{dir}', shellescape(l:dir), 'g')

    return l:cmd
endfunction

function! DefaultFileTestCommand()
    let l:defaults = {
        \ 'python': 'python -m pytest {file}',
        \ 'ruby': 'bundle exec rspec {file}',
        \ 'javascript': 'npm test -- {file}',
        \ 'typescript': 'npm test -- {file}',
        \ }

    return get(l:defaults, &filetype, '')
endfunction

function! GetFileTestCommand()
    if !exists('g:project_file_test_commands')
        let g:project_file_test_commands = {}
    endif

    let l:project_root = getcwd()

    if has_key(g:project_file_test_commands, l:project_root)
        return BuildTestCommand(g:project_file_test_commands[l:project_root])
    endif

    let l:default = $NVIM_FILE_TEST_COMMAND !=# ''
        \ ? $NVIM_FILE_TEST_COMMAND
        \ : DefaultFileTestCommand()

    if l:default ==# ''
        return ''
    endif

    return BuildTestCommand(l:default)
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
    let l:cmd = ''

    if CurrentFileLooksLikeTest()
        let l:cmd = GetFileTestCommand()
    endif

    if l:cmd ==# ''
        let l:cmd = GetTestCommand()
    endif

    if l:cmd ==# ''
        echo 'No test command set'
        return
    endif

    write
    let l:old_makeprg = &makeprg
    let &makeprg = l:cmd

    try
        make!

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

" Quick scratch
let g:quick_scratch_path = get(g:, 'quick_scratch_path', '~/SCRATCH.md')

function! QuickScratch() abort
  let l:scratch = resolve(fnamemodify(expand(g:quick_scratch_path), ':p'))
  let l:current = resolve(fnamemodify(expand('%:p'), ':p'))

  if l:current ==# l:scratch
    if exists('w:quick_scratch_last_bufnr') && bufexists(w:quick_scratch_last_bufnr)
      execute 'buffer' w:quick_scratch_last_bufnr
      unlet w:quick_scratch_last_bufnr
    else
      bprevious
    endif
    return
  endif

  let w:quick_scratch_last_bufnr = bufnr('%')
  execute 'edit' fnameescape(l:scratch)
  setlocal expandtab
endfunction

augroup quick_scratch_autosave
  autocmd!
  autocmd BufLeave,FocusLost,InsertLeave * call QuickScratchAutoSave()
augroup END

function! QuickScratchAutoSave() abort
  let l:scratch = resolve(fnamemodify(expand(g:quick_scratch_path), ':p'))
  let l:current = resolve(fnamemodify(expand('%:p'), ':p'))

  if l:current ==# l:scratch && &modified && !&readonly
    silent write
  endif
endfunction

nnoremap <silent> <M--> :call QuickScratch()<CR>
