local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

local plugins = {
    'tpope/vim-sensible',
    'tpope/vim-surround',
    'tpope/vim-rhubarb',
	'nvim-lua/plenary.nvim',
	{ 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
	{ 'nvim-telescope/telescope.nvim', tag = '0.1.6', requires = { {'nvim-lua/plenary.nvim'} } },
	{ 'ThePrimeagen/harpoon', branch = 'harpoon2', dependencies = {'nvim-lua/plenary.nvim'} },
	'mbbill/undotree',
	'tpope/vim-fugitive',

    -- LSP plugins
    'neovim/nvim-lspconfig',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/nvim-cmp',
    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig.nvim',

    -- 'junegunn/fzf', { 'do': { -> fzf#install() } }
    -- 'junegunn/fzf.vim',
    -- 'pechorin/any-jump.vim',

}

require('lazy').setup(plugins, {})
