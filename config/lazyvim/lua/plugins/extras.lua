local function check_aw_running()
	local handle = io.popen("nc -z localhost 5600 && echo open || echo closed")
	local result = handle:read("*a")
	handle:close()
	return result:find("open") ~= nil
end

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
	-- activity watch
	{
		"ActivityWatch/aw-watcher-vim",
		cmd = "AWStart",
		init = function()
			vim.g.aw_enabled = check_aw_running()
			if vim.g.aw_enabled then
				vim.cmd("AWStart")
			end
		end,
	},
	-- Vim-visual-multi - Multiple cursors in vim
	{
		"mg979/vim-visual-multi",
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
	-- direnv integration
	{
		"direnv/direnv.vim",
	},
	-- pencil colorscheme
	{
		"preservim/vim-colors-pencil",
	},
}
