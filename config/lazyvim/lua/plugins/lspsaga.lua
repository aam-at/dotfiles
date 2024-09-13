return {
	{
		"nvimdev/lspsaga.nvim",
		after = "nvim-lspconfig",
		config = function()
			require("lspsaga").setup({})
		end,
	},
	{
		"neovim/nvim-lspconfig",
		opts = function()
			local keys = require("lazyvim.plugins.lsp.keymaps").get()
			keys[#keys + 1] = { "K", "<cmd>Lspsaga hover_doc<cr>", desc = "Show doc" }
			keys[#keys + 1] = { "gd", "<cmd>Lspsaga goto_definition<cr>", desc = "Goto Definition" }
			keys[#keys + 1] = { "gD", "<cmd>Lspsaga goto_type_definition<cr>", desc = "Goto Declaration" }
			keys[#keys + 1] = { "gp", "<cmd>Lspsaga peek_definition<cr>", desc = "Peek Definition" }
			keys[#keys + 1] = { "gP", "<cmd>Lspsaga peek_type_definition<cr>", desc = "Peek Declaration" }
			keys[#keys + 1] = { "gr", "<cmd> Lspsaga finder<cr>", desc = "References" }
			keys[#keys + 1] = { "gR", "<cmd> Telescope lsp_references<cr>", desc = "References", nowait = true }
			keys[#keys + 1] = { "<leader>cO", "<cmd>Lspsaga outline<cr>", desc = "Toggle outline" }
		end,
	},
}
