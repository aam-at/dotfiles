-- Lua config

vim.api.nvim_set_keymap('i', '<C-CR>', 'copilot#Accept("<CR>")', { expr=true, noremap = true, silent = true })
vim.g.copilot_no_tab_map = true

local ok, treesitter = pcall(require, "nvim-treesitter.configs")
if ok then
	treesitter.setup({
		ensure_installed = { "lua", "markdown", "markdown_inline", "vim", "vimdoc" },
		highlight = { enable = true },
    indent = { enable = true },
	})
end

require("dressing").setup()
