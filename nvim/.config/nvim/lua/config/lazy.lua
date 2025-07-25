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

-- The following is the list of plugins from Plug:
-- Plug 'tpope/vim-sensible'
-- Plug 'tpope/vim-surround'
-- Plug 'tpope/vim-rhubarb'
--
-- Plug 'pechorin/any-jump.vim'
local plugins = {
  "nvim-lua/plenary.nvim",
  { "nvim-treesitter/nvim-treesitter",  build = ":TSUpdate" },
  {
    "nvim-telescope/telescope.nvim",
    tag = '0.1.6',
    requires = { { "nvim-lua/plenary.nvim" } }
  },
  {
    "ThePrimeagen/harpoon",
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim" }
  },
  { "mbbill/undotree" },
  { "tpope/vim-fugitive" },
  --lsp configuration
  { "neovim/nvim-lspconfig" },                                                                                                                                                --lsp configs
  { "hrsh7th/cmp-nvim-lsp" },                                                                                                                                                 -- autocompletion
  { "hrsh7th/nvim-cmp" },                                                                                                                                                     --additional autocompletion
  { "L3MON4D3/LuaSnip",                 version = "v2.*",   build = "make install_jsregexp", dependencies = { 'saadparwaiz1/cmp_luasnip', 'rafamadriz/friendly-snippets' } }, --snippet engine
  { "williamboman/mason.nvim" },                                                                                                                                              --lsp package manager
  { "williamboman/mason-lspconfig.nvim" },                                                                                                                                    --lsp package manager configs
}

require("lazy").setup(plugins, {})
