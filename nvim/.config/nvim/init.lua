-- init.lua - Main configuration file

vim.g.mapleader = " "

require('config.lazy')

-- Load modules
require('options')   -- General Neovim options
require('keymaps')   -- Key mappings
-- require('functions') -- Custom functions
-- require('autocmd')   -- Autocommands
-- require('lsp')
