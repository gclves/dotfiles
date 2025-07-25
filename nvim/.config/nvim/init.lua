-- init.lua - Main configuration file

-- the leader key is used in many keymaps
vim.g.mapleader = " "

require('config.lazy')
require('config.options') -- General Neovim options
require('keymaps')        -- Key mappings
require('functions')      -- Custom functions
require('autocmd')        -- Autocommands
