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

map('n', '<Leader>q', ':q<CR>', opts)

-- LSP keybindings
map('n', '<Leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
map('n', 'gd', '<cmd>lua GoToDefinition()<CR>', opts)
map('n', 'gr', '<cmd>lua GoToReferences()<CR>', opts)
--map('n', 'K', '<cmd>lua ShowDocs()<CR>', opts)

map('n', '<space>y', function() vim.fn.setreg('+', vim.fn.expand('%:p')) end)
map('n', '<space>c', function()
  vim.ui.input({ prompt = 'Command: ' }, function(cmd)
    if cmd and cmd ~= "" then
      -- 1. Define a width threshold (in columns) for vertical splitting.
      local vertical_split_threshold = 100

      -- 2. Get the width of the current window.
      local current_width = vim.fn.winwidth(0)

      -- 3. Decide whether to split vertically or horizontally.
      if current_width > vertical_split_threshold then
        vim.cmd('noswapfile vnew') -- It's wide, use a vertical split.
      else
        vim.cmd('noswapfile new')  -- It's narrow, use a horizontal split.
      end

      -- These commands run in the new window, regardless of split direction.
      vim.bo.buftype = 'nofile'
      vim.bo.bufhidden = "wipe"
      vim.api.nvim_buf_set_lines(0, 0, -1, false, vim.fn.systemlist(cmd))
    end
  end)
end)
