return {
	-- Oil - A modern, extensible, and fast file explorer for neovim
	{
		"stevearc/oil.nvim",
		cmd = "Oil",
		opts = {},
		dependencies = { { "echasnovski/mini.icons", opts = {} } },
		config = function(_, opts)
			require("oil").setup(opts)
		end,
		keys = {
			vim.keymap.set("n", "-", "<cmd>Oil<cr>", { desc = "Open parent directory" }),
		},
	},
	-- Vim-visual-multi - Multiple cursors in vim
	{
		"mg979/vim-visual-multi",
	},
	-- zen-mode - Distraction-free writing
	{
		"folke/zen-mode.nvim",
		cmd = "ZenMode",
		opts = {
			window = {
				width = 0.85,
			},
		},
		keys = {
			{ "<leader>uz", "<cmd>ZenMode<cr>", desc = "Zen Mode" },
		},
	},
	-- window picker
	{
		"s1n7ax/nvim-window-picker",
		name = "window-picker",
		event = "VeryLazy",
		version = "2.*",
		config = function()
			require("window-picker").setup()
		end,
	},
}
