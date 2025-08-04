-- options.lua - General Neovim options

-- General settings
vim.opt.compatible = false
vim.opt.encoding = 'utf-8'
vim.opt.fileencoding = 'utf-8'
vim.opt.hidden = true
vim.opt.history = 10000

vim.undodir = os.getenv("HOME") .. "/.vim.undodir"
vim.opt.undofile = true

vim.opt.backup = false
vim.opt.swapfile = false
vim.opt.showcmd = true
vim.opt.wildmenu = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.incsearch = true
vim.opt.hlsearch = true
vim.opt.scrolloff = 5
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.opt.wrap = true
vim.opt.linebreak = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smarttab = true
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.cindent = true
vim.opt.ruler = true
vim.opt.showmatch = true
vim.opt.laststatus = 2
vim.opt.lazyredraw = true
vim.opt.clipboard = 'unnamedplus'
vim.opt.backspace = 'indent,eol,start'
vim.opt.whichwrap:append('<,>,[,]')

vim.opt.colorcolumn = {80}
vim.opt.background = 'dark'
vim.cmd.colorscheme('default')

-- Folding (using Treesitter)
vim.opt.foldmethod = 'expr'
vim.opt.foldexpr = 'nvim_treesitter#foldexpr()'
vim.opt.foldlevel = 99

-- Set leader key
vim.g.mapleader = ' '

-- Enable syntax highlighting and filetype detection
vim.cmd('syntax on')
vim.cmd('filetype plugin indent on')

vim.diagnostic.config({
    virtual_text = { current_line = true }
})
