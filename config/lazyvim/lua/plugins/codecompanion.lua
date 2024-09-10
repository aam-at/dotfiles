return {
	{
		"olimorris/codecompanion.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter",
			"hrsh7th/nvim-cmp", -- Optional: For using slash commands and variables in the chat buffer
			"nvim-telescope/telescope.nvim", -- Optional: For working with files with slash commands
			{
				"stevearc/dressing.nvim", -- Optional: Improves the default Neovim UI
				opts = {},
			},
		},
		keys = {
			{ "<leader>a", "", desc = "+ai", mode = { "n", "v" } },
			{ "<leader>aC", "", desc = "Codecompanion", mode = { "n", "v" } },
			{
				"<leader>aCc",
				function()
					return require("codecompanion").chat()
				end,
				desc = "Codecompanion Chat",
				mode = { "n", "v" },
			},
			{
				"<leader>aCm",
				function()
					return require("codecompanion").actions()
				end,
				desc = "Codecompanion Actions",
				mode = { "n", "v" },
			},
			{
				"<leader>aCt",
				function()
					return require("codecompanion").toggle()
				end,
				desc = "Codecompanion Toggle",
				mode = { "n", "v" },
			},
			{
				"<leader>aCa",
				function()
					return require("codecompanion").add()
				end,
				desc = "Codecompanion Add",
				mode = { "v" },
			},
		},
		config = function()
			vim.defer_fn(function()
				require("codecompanion").setup({
					adapters = {
						openai = function()
							return require("codecompanion.adapters").extend("openai", {
								env = {
									api_key = "cmd:copy_password.sh apikey",
								},
							})
						end,
						gemini = function()
							return require("codecompanion.adapters").extend("gemini", {
								env = {
									api_key = "cmd:copy_password.sh gemini",
								},
							})
						end,
						deepseek = function()
							return require("codecompanion.adapters").extend("openai", {
								url = "https://api.deepseek.com/v1/chat/completions",
								env = {
									api_key = "cmd:copy_password.sh deepseek",
								},
								schema = {
									model = {
										order = 1,
										mapping = "parameters",
										type = "enum",
										default = "deepseek-chat",
										choices = {
											"deepseek-chat",
											"deepseek-coder",
										},
									},
								},
							})
						end,
					},
					strategies = {
						chat = {
							adapter = "copilot",
						},
						inline = {
							adapter = "copilot",
						},
						agent = {
							adapter = "copilot",
						},
					},
					opts = {
						log_level = "TRACE",
					},
				})
			end, 100)
		end,
	},

	-- Edgy integration
	{
		"folke/edgy.nvim",
		optional = true,
		opts = function(_, opts)
			opts.right = opts.right or {}
			table.insert(opts.right, {
				ft = "codecompanion",
				title = "CodeCompanion Chat",
				size = { width = 80 },
			})
		end,
	},
}
