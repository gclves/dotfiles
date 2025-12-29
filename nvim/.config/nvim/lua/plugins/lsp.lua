return {
    "neovim/nvim-lspconfig",
    dependencies = {
        "williamboman/mason.nvim",
        "williamboman/mason-lspconfig.nvim",
    },
    config = function()
        require('mason').setup()
        require('mason-lspconfig').setup({
            handlers = {
                function(server_name)
                    local capabilities = require('cmp_nvim_lsp').default_capabilities()

                    require('lspconfig')[server_name].setup({
                        capabilities = capabilities
                    })
                end,
            },
        })

        vim.api.nvim_create_autocmd('LspAttach', {
            callback = function(ev)
                local bufnr = ev.buf
                local opts = { buffer = bufnr, remap = false }

                vim.keymap.set('n', 'gd', function() vim.lsp.buf.definition() end, opts)
                vim.keymap.set('n', 'gr', function() vim.lsp.buf.references() end, opts)
                vim.keymap.set('n', 'K', function() vim.lsp.buf.hover() end, opts)
                vim.keymap.set('n', '<leader>vws', function() vim.lsp.buf.workspace_symbol() end, opts)
                vim.keymap.set('n', '<leader>vd', function() vim.diagnostic.open_foat() end, opts)
                vim.keymap.set('n', '[d', function() vim.diagnostic.goto_next() end, opts)
                vim.keymap.set('n', ']d', function() vim.diagnostic.goto_prev() end, opts)
                vim.keymap.set('n', '<leader>ca', function() vim.lsp.buf.code_action() end, opts)
                vim.keymap.set('n', '<leader>rn', function() vim.lsp.buf.rename() end, opts)
                vim.keymap.set('n', '<leader>h', function() vim.lsp.buf.signature_help() end, opts)

                vim.cmd [[autocmd BufWritePre <buffer> lua vim.lsp.buf.format()]]

                local client = vim.lsp.get_client_by_id(ev.data.client_id)
                if client:supports_method('textDocument/completion') then
                    vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = false })
                end
            end,
        })
        vim.api.nvim_create_user_command('Format',
            function()
                vim.lsp.buf.format()
            end,
            {}
        )
    end,
}
