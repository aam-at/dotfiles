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
	-- tmux navigator
	{
		"christoomey/vim-tmux-navigator",
		cmd = {
			"TmuxNavigateLeft",
			"TmuxNavigateDown",
			"TmuxNavigateUp",
			"TmuxNavigateRight",
			"TmuxNavigatePrevious",
		},
		keys = {
			{ "<c-h>", "<cmd><C-U>TmuxNavigateLeft<cr>" },
			{ "<c-j>", "<cmd><C-U>TmuxNavigateDown<cr>" },
			{ "<c-k>", "<cmd><C-U>TmuxNavigateUp<cr>" },
			{ "<c-l>", "<cmd><C-U>TmuxNavigateRight<cr>" },
			{ "<c-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>" },
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
