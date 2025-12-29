-- autocmds.lua - Autocommands

local function augroup(name)
    return vim.api.nvim_create_augroup("lazyvim_" .. name, { clear = true })
end

-- 1. Highlight on Yank
-- Visually highlights the text you just copied for 200ms.
-- It's a great visual indicator that the copy action actually worked.
vim.api.nvim_create_autocmd("TextYankPost", {
    group = augroup("highlight_yank"),
    callback = function()
        vim.highlight.on_yank({ higroup = "IncSearch", timeout = 200 })
    end,
})

-- 2. Resize Splits Automatically
-- If you resize your terminal window, this automatically balances
-- the splits so they don't look crushed.
vim.api.nvim_create_autocmd({ "VimResized" }, {
    group = augroup("resize_splits"),
    callback = function()
        local current_tab = vim.fn.tabpagenr()
        vim.cmd("tabdo wincmd =")
        vim.cmd("tabnext " .. current_tab)
    end,
})

-- 3. Check if file changed on disk
-- If you modify a file externally (e.g., git pull in another terminal),
-- this checks and reloads it automatically when you focus Neovim.
vim.api.nvim_create_autocmd({ "FocusGained", "TermClose", "TermLeave" }, {
    group = augroup("checktime"),
    callback = function()
        if vim.o.buftype ~= "nofile" then
            vim.cmd("checktime")
        end
    end,
})

-- 4. Restore Cursor Position
-- When opening a file, jump to the last place you were editing
-- (unless it's a git commit message).
vim.api.nvim_create_autocmd("BufReadPost", {
    group = augroup("last_loc"),
    callback = function(event)
        local exclude = { "gitcommit" }
        local buf = event.buf
        if vim.tbl_contains(exclude, vim.bo[buf].filetype) then
            return
        end
        local mark = vim.api.nvim_buf_get_mark(buf, '"')
        local lcount = vim.api.nvim_buf_line_count(buf)
        if mark[1] > 0 and mark[1] <= lcount then
            pcall(vim.api.nvim_win_set_cursor, 0, mark)
        end
    end,
})

-- Close help windows with 'q'
local help_group = augroup('HelpMappings', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
    group = help_group,
    pattern = 'help',
    callback = function()
        vim.keymap.set('n', 'q', ':q<CR>', { buffer = true })
    end,
})

-- Format on save
local format_group = augroup('FormatOnSave', { clear = true })
vim.api.nvim_create_autocmd('BufWritePre', {
    group = format_group,
    callback = function()
        vim.cmd('Format')
    end,
})
