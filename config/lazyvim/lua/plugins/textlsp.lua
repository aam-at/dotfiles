local _, openai_api_key = RunShellCommand("copy_password.sh apikey")

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
										on_open = false,
										on_save = false,
										on_change = false,
									},
								},
								openai = {
									enabled = true,
									api_key = openai_api_key,
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
