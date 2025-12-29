local opt = vim.opt

-- 1. UI & Visuals
opt.number = true         -- Show line numbers
opt.relativenumber = true -- Relative line numbers
opt.cursorline = true     -- Highlight the current line
opt.signcolumn = "yes"    -- Always show the sign column (prevents text shifting)
opt.laststatus = 2        -- Always show status line
opt.showmode = false      -- Don't show mode in command line (plugins usually handle this)
opt.termguicolors = true  -- True color support
opt.scrolloff = 8         -- Keep 8 lines above/below cursor (Updated from 5 for better view)
opt.wrap = true           -- Wrap long lines...
opt.linebreak = true      -- ...but break at words
opt.colorcolumn = "80"    -- Visual guide at column 80

-- 2. Indentation (Tabs = 4 spaces)
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true   -- Convert tabs to spaces
opt.smartindent = true -- Smart auto-indenting

-- 3. Search
opt.ignorecase = true -- Ignore case when searching...
opt.smartcase = true  -- ...unless you type a capital letter
opt.incsearch = true  -- Show search matches as you type
opt.hlsearch = true   -- Highlight search results

-- 4. System & Files
opt.clipboard = "unnamedplus" -- Sync with system clipboard
opt.mouse = "a"               -- Enable mouse support
opt.undofile = true           -- Save undo history to disk
opt.swapfile = false          -- Disable swap files
opt.backup = false            -- Disable backup files
opt.hidden = true             -- Allow hiding buffers with unsaved changes
opt.history = 1000            -- Command history size

-- 5. Undo Storage (Robust handling)
-- Using stdpath("state") keeps your HOME folder clean
opt.undodir = vim.fn.stdpath("state") .. "/undo"

-- 6. Folding (Treesitter)
opt.foldmethod = "expr"
opt.foldexpr = "nvim_treesitter#foldexpr()"
opt.foldlevel = 99 -- Start with all folds open

-- 7. Diagnostics (Global Config)
vim.diagnostic.config({
    virtual_text = true,      -- Show inline errors
    signs = true,             -- Show icons in the gutter
    underline = true,
    update_in_insert = false, -- Don't scream errors while typing
    severity_sort = true,
})
