return {
	{
		"nvim-orgmode/orgmode",
		dependencies = {
			{
				"nvim-treesitter/nvim-treesitter",
				config = function()
					require("nvim-treesitter.configs").setup({
						ensure_installed = { "org" },
					})
				end,
			},
		},
		event = "VeryLazy",
		ft = { "org" },
		config = function()
			-- Setup orgmode
			require("orgmode").setup({
				org_agenda_files = "~/Dropbox/Org/**/*",
				org_default_notes_file = "~/Dropbox/Org/refile.org",
			})
		end,
	},
	{
		"chipsenkbeil/org-roam.nvim",
		dependencies = {
			{
				"nvim-orgmode/orgmode",
			},
		},
		event = "VeryLazy",
		ft = { "org" },
		config = function()
			require("org-roam").setup({
				directory = "~/Dropbox/Org/",
			})
		end,
	},
	{
		"bi0ha2ard/telescope-org_roam.nvim",
		requires = {
			{ "nvim-telescope/telescope.nvim" },
		},
	},
}
