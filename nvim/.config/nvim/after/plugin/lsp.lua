require("mason").setup({})
require("mason-lspconfig").setup({
  handlers = {
    function(server_name)
      local capabilities = require("cmp_nvim_lsp").default_capabilities()
      require("lspconfig")[server_name].setup({
        capabilities = capabilities
      })
    end,
  },
  ensure_installed = {
    'lua_ls',
    'rust_analyzer'
  }
})

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local bufnr = args.buf
    local opts = { buffer = bufnr, remap = false }
    local map = vim.keymap.set

    map("n", "gd", function() vim.lsp.buf.definition() end, opts)
    map('n', 'gr', function() vim.lsp.buf.references() end, opts)
    map('n', 'K', function() vim.lsp.buf.hover() end, opts)
    map('n', '<leader>vws', function() vim.lsp.buf.workspace_symbol() end, opts)
    map('n', '<leader>vd', function() vim.diagnostic.open_foat() end, opts)
    map('n', '[d', function() vim.diagnostic.goto_prev() end, opts)
    map('n', ']d', function() vim.diagnostic.goto_next() end, opts)
    map('n', '<leader>.', function() vim.lsp.buf.code_action() end, opts)
    map('n', '<leader>rn', function() vim.lsp.buf.rename() end, opts)
    map('n', '<leader>h', function() vim.lsp.buf.signature_help() end, opts)

    -- map('n', 'gd', '<cmd>lua GoToDefinition()<CR>', opts)
    -- map('n', 'gr', '<cmd>lua GoToReferences()<CR>', opts)
  end
})
