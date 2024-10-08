local M = {}

---@param kind string
function M.pick(kind)
	return function()
		local actions = require("CopilotChat.actions")
		local items = actions[kind .. "_actions"]()
		if not items then
			LazyVim.warn("No " .. kind .. " found on the current line")
			return
		end
		local ok = pcall(require, "fzf-lua")
		require("CopilotChat.integrations." .. (ok and "fzflua" or "telescope")).pick(items)
	end
end

return {
	{
		"CopilotC-Nvim/CopilotChat.nvim",
		branch = "canary",
		cmd = "CopilotChat",
		opts = function()
			local user = vim.env.USER or "User"
			user = user:sub(1, 1):upper() .. user:sub(2)
			return {
				model = "gpt-4",
				auto_insert_mode = true,
				show_help = true,
				question_header = "  " .. user .. " ",
				answer_header = "  Copilot ",
				window = {
					width = 0.4,
				},
				selection = function(source)
					local select = require("CopilotChat.select")
					return select.visual(source) or select.buffer(source)
				end,
			}
		end,
		keys = {
			{ "<c-s>", "<CR>", ft = "copilot-chat", desc = "CopilotChat Submit Prompt", remap = true },
			{ "<leader>a", "", desc = "+ai", mode = { "n", "v" } },
			{ "<leader>ac", "", desc = "CopilotChat", mode = { "n", "v" } },
			{
				"<leader>aca",
				function()
					return require("CopilotChat").toggle()
				end,
				desc = "CopilotChat Toggle",
				mode = { "n", "v" },
			},
			{
				"<leader>acx",
				function()
					return require("CopilotChat").reset()
				end,
				desc = "CopilotChat Clear",
				mode = { "n", "v" },
			},
			{
				"<leader>acq",
				function()
					local input = vim.fn.input("Quick Chat: ")
					if input ~= "" then
						require("CopilotChat").ask(input)
					end
				end,
				desc = "CopilotChat Quick Chat",
				mode = { "n", "v" },
			},
			-- Show help actions with telescope
			{ "<leader>acd", M.pick("help"), desc = "Diagnostic Help", mode = { "n", "v" } },
			-- Show prompts actions with telescope
			{ "<leader>acp", M.pick("prompt"), desc = "Prompt Actions", mode = { "n", "v" } },
		},
		config = function(_, opts)
			local chat = require("CopilotChat")
			require("CopilotChat.integrations.cmp").setup()

			vim.api.nvim_create_autocmd("BufEnter", {
				pattern = "copilot-chat",
				callback = function()
					vim.opt_local.relativenumber = false
					vim.opt_local.number = false
				end,
			})

			chat.setup(opts)
		end,
	},

	-- Edgy integration
	{
		"folke/edgy.nvim",
		optional = true,
		opts = function(_, opts)
			opts.right = opts.right or {}
			table.insert(opts.right, {
				ft = "copilot-chat",
				title = "Copilot Chat",
				size = { width = 80 },
			})
		end,
	},
}
