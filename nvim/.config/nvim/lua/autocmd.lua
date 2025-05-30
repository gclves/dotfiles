-- autocmds.lua - Autocommands

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

-- Clean whitespace on save
local whitespace_group = augroup('WhitespaceCleanup', { clear = true })
autocmd('BufWritePre', {
    group = whitespace_group,
    callback = function()
        CleanWhitespace()
    end,
})

-- Close help windows with 'q'
local help_group = augroup('HelpMappings', { clear = true })
autocmd('FileType', {
    group = help_group,
    pattern = 'help',
    callback = function()
        vim.keymap.set('n', 'q', ':q<CR>', { buffer = true })
    end,
})

-- Format on save
local format_group = augroup('FormatOnSave', { clear = true })
autocmd('BufWritePre', {
    group = format_group,
    callback = function()
        vim.cmd('Format')
    end,
})
