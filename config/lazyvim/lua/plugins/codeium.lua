return {
	-- codeium
	{
		"Exafunction/codeium.nvim",
		cmd = "Codeium",
		build = ":Codeium Auth",
		config = function()
			require("codeium").setup({
				enable_chat = true,
			})
		end,
	},
	-- codeium cmp source
	{
		"nvim-cmp",
		opts = function(_, opts)
			table.insert(opts.sources, 1, {
				name = "codeium",
				group_index = 1,
				priority = 100,
			})
		end,
	},
	-- lualine integration
	{
		"nvim-lualine/lualine.nvim",
		optional = true,
		event = "VeryLazy",
		opts = function(_, opts)
			table.insert(opts.sections.lualine_x, 2, LazyVim.lualine.cmp_source("codeium"))
		end,
	},
}
