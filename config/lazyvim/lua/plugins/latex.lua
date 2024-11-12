return {
	{
		"williamboman/mason.nvim",
		opts = function(_, opts)
			table.insert(opts.ensure_installed, "ltex-ls")
			table.insert(opts.ensure_installed, "vale")
		end,
	},
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

			vim.g.vimtex_view_method = "zathura"
			vim.g.vimtex_view_method_sync = 1
			vim.g.vimtex_view_method_activate = 1
			vim.g.vimtex_view_method_reading_bar = 1

			vim.g.vimtex_compiler_latexmk = {
				aux_dir = "./aux",
				out_dir = "./out",
			}
		end,
	},
	{
		"let-def/texpresso.vim",
		commands = {
			"TeXpresso",
		},
		config = function()
			vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
				group = vim.api.nvim_create_augroup("lazyvim_texpresso_keymap", { clear = true }),
				pattern = { "*.tex" },
				callback = function()
					vim.api.nvim_buf_set_keymap(
						0,
						"n",
						"<leader>cx",
						":TeXpresso %<CR>",
						{ noremap = true, silent = true }
					)
				end,
			})
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
		config = function()
			require("telescope").load_extension("bibtex")
			require("telescope").setup({
				extensions = {
					bibtex = {
						global_files = {
							"~/Dropbox/Research/Bibliography/refs.bib",
							"~/Dropbox/Research/Bibliography/myrefs.bib",
						},
					},
				},
			})
		end,
	},
	{
		"mfussenegger/nvim-lint",
		optional = true,
		opts = {
			linters_by_ft = {
				org = { "vale" },
				txt = { "vale" },
				latex = { "vale", "chktex" },
				markdown = { "vale" },
			},
		},
	},
}
