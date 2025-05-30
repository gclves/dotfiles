-- LSP setup
require('mason').setup()
require('mason-lspconfig').setup({
    ensure_installed = {
        'lua_ls',        -- Lua
        'gopls',         -- Go
        'rust_analyzer', -- Rust
        'elixirls'       -- Elixir
    }
})

-- Basic LSP server configurations
local lspconfig = require('lspconfig')
lspconfig.lua_ls.setup({})
lspconfig.gopls.setup({})
lspconfig.rust_analyzer.setup({})
lspconfig.elixirls.setup({})
lspconfig.ts_ls.setup({})

vim.api.nvim_create_autocmd('LspAttach', {
    callback = function(ev)
        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        if client:supports_method('textDocument/completion') then
            vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = false })
        end
    end,
})
