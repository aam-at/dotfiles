return {
	{
		"folke/snacks.nvim",
		keys = {
			{ "-", function() Snacks.explorer() end, desc = "Open Explorer" },
			{ "<leader>uz", function() Snacks.zen() end, desc = "Zen Mode" },
		},
		opts = {
			explorer = { enabled = true },
			image = { enabled = true },
			indent = { enabled = true, scope = { enabled = true } },
			notifier = { enabled = true, timeout = 3000 },
			words = { enabled = true },
			zen = {
				toggles = { dim = true, git_signs = false, mini_diff_signs = false },
				show = { statusline = false, tabline = false },
				win = {
					width = 0.65,
					wo = {
						cursorcolumn = false,
						cursorline = false,
						linebreak = true,
						number = false,
						relativenumber = false,
						signcolumn = "no",
						wrap = true,
					},
				},
			},
			dashboard = {
				sections = {
					{ section = "header" },
					{ section = "keys", gap = 1, indent = 2, padding = 1 },
					{ icon = " ", title = "Recent Files", section = "recent_files", indent = 2, padding = 1 },
					{ icon = " ", title = "Projects", section = "projects", indent = 2, padding = 1 },
					{ section = "startup" },
				},
			},
		},
		init = function()
			vim.api.nvim_create_user_command("ZenMode", function() Snacks.zen() end, {})
		end,
	},
}
