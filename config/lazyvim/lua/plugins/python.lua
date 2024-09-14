return {
	-- mason to automatically install ruff
	{
		"williamboman/mason.nvim",
		opts = function(_, opts)
			table.insert(opts.ensure_installed, "ruff")
		end,
	},
	-- configure ruff as a formatter for python
	{
		"nvimtools/none-ls.nvim",
		optional = true,
		opts = function(_, opts)
			local nls = require("null-ls")
			opts.sources = opts.sources or {}
			table.insert(opts.sources, nls.builtins.formatting.ruff)
		end,
	},
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
