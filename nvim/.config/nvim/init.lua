-- init.lua - Main configuration file
-- 1. Set Leader Key
-- Must happen before plugins are loaded (otherwise wrong leader will be used in mappings)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- 2. Load Core Configuration
require("config.options")
require("config.keymaps")
require("config.autocmd")

-- 3. Load Plugin Manager
require("config.lazy")
