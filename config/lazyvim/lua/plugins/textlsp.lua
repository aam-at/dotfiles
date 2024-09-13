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
									enabled = false,
								},
								gramformer = {
									-- gramformer dependency needs to be installed manually
									enabled = true,
									gpu = true,
									check_text = {
										on_open = true,
										on_save = true,
										on_change = false,
									},
								},
								openai = {
									enabled = false,
									api_key = "api_key",
									-- url = '<CUSTOM_URL>'  -- optional to use an OpenAI-compatible server
									check_text = {
										on_open = false,
										on_save = false,
										on_change = false,
									},
									model = "gpt-4o-mini",
									max_token = 50,
								},
							},
						},
					},
				},
			},
		},
	},
}
