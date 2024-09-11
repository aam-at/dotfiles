return {
	{
		"williamboman/mason.nvim",
		opts = function(_, opts)
			table.insert(opts.ensure_installed, "textlsp")
		end,
	},
	{
		"neovim/nvim-lspconfig",
		opts = {
			servers = {
				textlsp = {
					filetypes = { "tex", "latex", "pandoc", "markdown" },
					settings = {
						textLSP = {
							analysers = {
								languagetool = {
									enabled = true,
								},
							},
						},
					},
				},
			},
		},
	},
}
