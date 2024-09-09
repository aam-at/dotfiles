return {
	{
		"lervag/vimtex",
		lazy = false, -- lazy-loading will disable inverse search
		config = function()
			vim.api.nvim_create_autocmd({ "FileType" }, {
				group = vim.api.nvim_create_augroup("lazyvim_vimtex_conceal", { clear = true }),
				pattern = { "bib", "tex" },
				callback = function()
					vim.wo.conceallevel = 0
				end,
			})
			vim.g.vimtex_mappings_disable = { ["n"] = { "K" } } -- disable `K` as it conflicts with LSP hover
			vim.g.vimtex_quickfix_method = vim.fn.executable("pplatex") == 1 and "pplatex" or "latexlog"

			vim.g.vimtex_view_method = "skim" -- <== macos specific, you can use zathura or sumatra or something else.
			vim.g.vimtex_view_skim_sync = 1
			vim.g.vimtex_view_skim_activate = 1
			vim.g.vimtex_view_skim_reading_bar = 1

			vim.g.vimtex_compiler_latexmk = {
				aux_dir = "./aux",
				out_dir = "./out",
			}
		end,
	},
	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			{ "kdheepak/cmp-latex-symbols" },
		},
		opts = function(_, opts)
			table.insert(opts.sources, {
				name = "latex_symbols",
			})
		end,
	},
	{
		"nvim-telescope/telescope-bibtex.nvim",
		requires = {
			{ "nvim-telescope/telescope.nvim" },
		},
	},
}
