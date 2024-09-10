return {
	-- Oil - A modern, extensible, and fast file explorer for neovim
	{
		"stevearc/oil.nvim",
		opts = {},
		dependencies = { { "echasnovski/mini.icons", opts = {} } },
		config = function(_, opts)
			require("oil").setup(opts)
		end,
	},
	-- Vim-visual-multi - Multiple cursors in vim
	{
		"mg979/vim-visual-multi",
	},
	-- zen-mode - Distraction-free writing
	{
		"folke/zen-mode.nvim",
		opts = {
			window = {
				width = 0.85,
			},
		},
	},
}
