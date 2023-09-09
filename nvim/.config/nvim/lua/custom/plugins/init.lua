vim.keymap.set('i', 'jk', '<Esc>', {})
vim.keymap.set('n', '<BS>', '%', {})

-- See the kickstart.nvim README for more information
return {
	{
		"nvim-neo-tree/neo-tree.nvim",
		keys = {
			{ "<leader>ft", "<cmd>Neotree toggle<cr>", desc = "NeoTree" },
		},
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons",
			"MunifTanjim/nui.nvim",
		},
		config = function()
			require("neo-tree").setup()
		end,
	},
	{
		'vim-test/vim-test',
		keys = {
			{ "<leader>tt", "<cmd>TestLast<cr>",    desc = "Test last" },
			{ "<leader>tn", "<cmd>TestNearest<cr>", desc = "[T]est [N]earest" },
			{ "<leader>tf", "<cmd>TestFile<cr>",    desc = "[T]est [F]ile" },
			{ "<leader>ta", "<cmd>TestSuite<cr>",   desc = "[T]est [A]ll Suite" },
			{ "<leader>tg", "<cmd>TestVisit<cr>",   desc = "[T]est [G]oto" },
		},
		config = function()
			vim.g["test#strategy"] = "kitty"
			-- vim.g["test#strategy"] = "neovim"
		end,
	},
	{
		'https://gitlab.com/ajgrf/parchment',
		priority = 1000,
		config = function()
			vim.cmd.colorscheme 'parchment'
		end
	}
}
