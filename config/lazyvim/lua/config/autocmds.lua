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

-- Custom Local Functions

-- Split selected text into one sentence per line
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

-- Custom Global Functions

-- Function to Run Shell Command
function RunShellCommand(cmd)
	local handle, err = io.popen(cmd)
	if not handle then
		return false, err
	end

	local result, read_err = handle:read("*a")
	local _, close_err = handle:close()

	if read_err or close_err then
		return false, read_err or close_err
	end

	result = string.gsub(result, "[\r\n]+$", "")
	return true, result
end

-- Function to load and parse a JSON file
function LoadJSONFile(filepath)
	-- Expand the path to handle ~
	filepath = filepath:gsub("~", os.getenv("HOME"))
	-- Open the file in read mode
	local file = io.open(filepath, "r")

	-- Check if file exists
	if not file then
		print("Error: Could not open file " .. filepath)
		return nil
	end

	-- Read the entire file contents
	local content = file:read("*all")
	file:close()

	-- Require the vim.json module for parsing
	local json = vim.json

	-- Parse the JSON content
	local success, result = pcall(function()
		return json.decode(content)
	end)

	-- Check if parsing was successful
	if not success then
		print("Error parsing JSON: " .. tostring(result))
		return nil
	end

	return result
end
