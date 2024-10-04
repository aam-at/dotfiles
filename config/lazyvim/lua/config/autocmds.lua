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

-- custom functions
local function split_to_one_sentence_per_line(opts)
	local start_line, start_col, end_line, end_col

	-- Visual mode selection
	start_line, start_col = opts.line1, 1
	end_line, end_col = opts.line2, vim.fn.col("'>")

	local lines = vim.api.nvim_buf_get_lines(0, start_line - 1, end_line, false)

	-- Adjust the last line to respect the end column
	if #lines > 0 then
		lines[#lines] = lines[#lines]:sub(1, end_col)
	end

	local text = table.concat(lines, "\n")

	-- Remove existing newlines and extra spaces
	text = text:gsub("\n", " "):gsub("%s+", " ")

	-- Split into sentences
	local sentences = {}
	for sentence in text:gmatch("([^.!?]+[.!?])") do
		table.insert(sentences, sentence:match("^%s*(.-)%s*$"))
	end

	-- Replace the selected text with the split sentences
	vim.api.nvim_buf_set_lines(0, start_line - 1, end_line, false, sentences)
end

-- Create a Vim command to call the function
vim.api.nvim_create_user_command("SplitSentences", split_to_one_sentence_per_line, { range = true })
