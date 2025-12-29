return {
    {
        "tpope/vim-fugitive",
        config = function()
            vim.keymap.set("n", "<leader>gs", vim.cmd.Git)
        end,
    },
    "tpope/vim-rhubarb", -- GitHub integration for fugitive
}
