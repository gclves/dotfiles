return {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
        local configs = require("nvim-treesitter.configs")

        configs.setup({
            ensure_installed = {
                "c", "lua", "vim", "vimdoc", "query", "markdown",
                "markdown_inline", "elixir", "typescript", "eex", "heex"
            },

            -- Install parsers synchronously (only applied to `ensure_installed`)
            sync_install = false,

            -- Automatically install missing parsers when entering buffer
            auto_install = true,

            ignore_install = { "javascript" },

            highlight = {
                enable = true, -- false will disable the whole extension
                additional_vim_regex_highlighting = false,
                disable = function(_lang, buf)
                    local max_filesize = 100 * 1024
                    local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
                    if ok and stats and stats.size < max_filesize then
                        return true
                    end
                end,
            },

            indent = { enable = true },
        })
    end,
}
