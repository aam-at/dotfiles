return {
	-- mason to automatically install ruff and basedpyright
	{
		"williamboman/mason.nvim",
		opts = function(_, opts)
			table.insert(opts.ensure_installed, "ruff")
			table.insert(opts.ensure_installed, "basedpyright")
		end,
	},
	-- configure ruff as a formatter for python
	{
		"stevearc/conform.nvim",
		optional = true,
		opts = {
			formatters_by_ft = {
				["python"] = { "ruff" },
			},
		},
	},
	-- automatically resolve imports for pyright server
	{
		"stevanmilic/nvim-lspimport",
		config = function(_, opts)
			vim.api.nvim_create_autocmd("FileType", {
				pattern = "python",
				callback = function()
					local fixImport = require("lspimport").import
					vim.keymap.set(
						"n",
						"<leader>ci",
						fixImport,
						{ desc = "Fix imports", noremap = true, silent = true }
					)
				end,
			})
		end,
	},
}
