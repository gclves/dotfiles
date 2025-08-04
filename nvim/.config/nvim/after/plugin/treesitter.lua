require('nvim-treesitter.configs').setup({
    -- A list of parser names, or "all"
    ensure_installed = {
        "c", "lua", "vim", "vimdoc", "query", "markdown",
        "markdown_inline", "elixir", "typescript", "eex", "heex"
    },

    -- Install parsers synchronously (only applied to `ensure_installed`)
    sync_install = false,

    -- Automatically install missing parsers when entering buffer
    auto_install = true,

    -- List of parsers to ignore installing (or "all")
    ignore_install = { "javascript" },

    highlight = {
        enable = true,
	disable = {},
	disable = function(lang, buf)
		local max_filesize = 100 * 1024
		local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
		if ok and stats and stats.size < max_filesize then
			return true
		end
	end,
        -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
        -- Using this option may slow down your editor, and you may see some duplicate highlights.
        additional_vim_regex_highlighting = false,
    },
})

