-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
-- ("-" -> Oil is defined as a lazy-loading key trigger in plugins/extras.lua)

--- Bufferline keymaps (LazyVim default tabline)
local opts = { noremap = true, silent = true }
-- Move to previous/next
vim.keymap.set("n", "<A-,>", "<Cmd>BufferLineCyclePrev<CR>", opts)
vim.keymap.set("n", "<A-.>", "<Cmd>BufferLineCycleNext<CR>", opts)
-- Re-order to previous/next
vim.keymap.set("n", "<A-<>", "<Cmd>BufferLineMovePrev<CR>", opts)
vim.keymap.set("n", "<A->>", "<Cmd>BufferLineMoveNext<CR>", opts)
-- Goto buffer in position...
vim.keymap.set("n", "<A-1>", "<Cmd>BufferLineGoToBuffer 1<CR>", opts)
vim.keymap.set("n", "<A-2>", "<Cmd>BufferLineGoToBuffer 2<CR>", opts)
vim.keymap.set("n", "<A-3>", "<Cmd>BufferLineGoToBuffer 3<CR>", opts)
vim.keymap.set("n", "<A-4>", "<Cmd>BufferLineGoToBuffer 4<CR>", opts)
vim.keymap.set("n", "<A-5>", "<Cmd>BufferLineGoToBuffer 5<CR>", opts)
vim.keymap.set("n", "<A-6>", "<Cmd>BufferLineGoToBuffer 6<CR>", opts)
vim.keymap.set("n", "<A-7>", "<Cmd>BufferLineGoToBuffer 7<CR>", opts)
vim.keymap.set("n", "<A-8>", "<Cmd>BufferLineGoToBuffer 8<CR>", opts)
vim.keymap.set("n", "<A-9>", "<Cmd>BufferLineGoToBuffer 9<CR>", opts)
vim.keymap.set("n", "<A-0>", "<Cmd>BufferLineGoToBuffer -1<CR>", opts)
