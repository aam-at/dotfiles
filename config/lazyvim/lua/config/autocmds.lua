-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

local function augroup(name)
	return vim.api.nvim_create_augroup("lazyvim_" .. name, { clear = true })
end

vim.api.nvim_create_autocmd("FileType", {
	group = augroup("custom_text_fonts"),
	pattern = {
		"markdown",
		"mkd",
		"org",
		"tex",
	},
	callback = function()
		vim.opt_local.guifont = "iA Writer Mono V:h14"
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	group = augroup("custom_prog_fonts"),
	pattern = {
		"lisp",
		"lua",
		"python",
	},
	callback = function()
		vim.opt_local.guifont = "JetBrains Mono:h12"
	end,
})
