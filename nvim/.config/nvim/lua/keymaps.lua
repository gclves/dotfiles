-- keymaps.lua - Key mappings

local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- Exit insert mode with 'jk' or 'kj'
map('i', 'jk', '<Esc>', opts)
map('i', 'kj', '<Esc>', opts)

-- Better line navigation for wrapped lines
map('n', 'k', 'gk', opts)
map('n', 'j', 'gj', opts)

-- Navigate errors
map('n', '<C-n>', ':cn<CR>', opts)
map('n', '<C-p>', ':cp<CR>', opts)

-- Window navigation
map('n', '<C-h>', '<C-w>h', opts)
map('n', '<C-j>', '<C-w>j', opts)
map('n', '<C-k>', '<C-w>k', opts)
map('n', '<C-l>', '<C-w>l', opts)

-- Clear search highlighting
map('n', '<Escape>', ':nohlsearch<CR>', opts)

-- Swap : and ; for easier command entry
map('n', ';', ':', { noremap = true })
map('n', ':', ';', { noremap = true })

-- Edit init.lua (equivalent to .vimrc)
map('n', '<Leader>ev', ':tabe $MYVIMRC<CR>', opts)

-- FZF mappings
map('n', '<Leader>f', ':Files<CR>', opts)
map('n', '<Leader>b', ':Buffers<CR>', opts)
map('n', '<Leader>/', '<cmd>Rg<CR>', opts)

-- Save with Ctrl+s
map('i', '<C-s>', '<Esc>:w<CR>a', opts)
map('n', '<C-s>', ':w<CR>', opts)

-- New tab with Ctrl+t
map('i', '<C-t>', '<Esc>:tabnew<CR>', opts)

-- Run file
map('n', '<Leader>r', '<cmd>lua RunFile()<CR>', opts)

-- Close the buffer without closing the split
map('n', '<Leader>d', ':b#<bar>bd#<CR>', opts)

-- LSP keybindings
map('n', '<Leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
map('n', 'gd', '<cmd>lua GoToDefinition()<CR>', opts)
map('n', 'gr', '<cmd>lua GoToReferences()<CR>', opts)
--map('n', 'K', '<cmd>lua ShowDocs()<CR>', opts)
