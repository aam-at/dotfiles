return {
	{
		"nvim-orgmode/orgmode",
		dependencies = {
			{
				"nvim-treesitter/nvim-treesitter",
				config = function()
					require("nvim-treesitter.configs").setup({
						ensure_installed = { "norg" },
					})
				end,
			},
			{
				"hrsh7th/nvim-cmp",
				optional = true,
				opts = function(_, opts)
					table.insert(opts.sources, 1, {
						name = "orgmode",
					})
				end,
			},
		},
		event = "VeryLazy",
		ft = { "org" },
		config = function()
			-- Setup orgmode
			require("orgmode").setup({
				org_agenda_files = {
					"~/Dropbox/Org/inbox.org",
					"~/Dropbox/Org/someday.org",
					"~/Dropbox/Org/work.org",
					"~/Dropbox/Org/personal.org",
					"~/Dropbox/Org/ideas.org",
					"~/Dropbox/Org/projects/**",
				},
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
