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
		branch = "main",
		cmd = "CopilotChat",
		opts = function()
			local user = vim.env.USER or "User"
			user = user:sub(1, 1):upper() .. user:sub(2)
			local opts = {
				model = "gpt-4o",
				auto_insert_mode = true,
				show_help = true,
				question_header = "  " .. user .. " ",
				answer_header = "  Copilot ",
				window = {
					width = 0.4,
				},
			}
			-- load prompts from a json file
			local prompts_data = LoadJSONFile("~/Dropbox/Org/data/prompts/prompts.json")
			-- convert the json data to a list of prompts
			local prompts_list = {}
			for key, value in pairs(prompts_data) do
				prompts_list[key] = {
					system_prompt = value,
				}
			end
			opts.prompts = prompts_list
			return opts
		end,
		keys = {
			{ "<c-s>", "<CR>", ft = "copilot-chat", desc = "Submit Prompt", remap = true },
			{ "<leader>a", "", desc = "+ai", mode = { "n", "v" } },
			{ "<leader>ac", "", desc = "CopilotChat", mode = { "n", "v" } },
			{
				"<leader>aca",
				function()
					return require("CopilotChat").toggle()
				end,
				desc = "Toggle (CopilotChat)",
				mode = { "n", "v" },
			},
			{
				"<leader>acx",
				function()
					return require("CopilotChat").reset()
				end,
				desc = "Clear (CopilotChat)",
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
				desc = "Quick Chat (CopilotChat)",
			},
			{
				"<leader>acm",
				function()
					require("CopilotChat").select_model()
				end,
				desc = "Select Model (CopilotChat)",
				mode = { "n", "v" },
			},
			-- Show help actions with telescope
			{ "<leader>acd", M.pick("help"), desc = "Diagnostic Help (CopilotChat)", mode = { "n", "v" } },
			-- Show prompts actions with telescope
			{ "<leader>acp", M.pick("prompt"), desc = "Prompt Actions (CopilotChat)", mode = { "n", "v" } },
		},
		config = function(_, opts)
			local chat = require("CopilotChat")

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
