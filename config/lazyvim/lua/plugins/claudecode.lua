return {
  "coder/claudecode.nvim",
  opts = {},
  keys = {
    { "<leader>a", "", desc = "+ai", mode = { "n", "v" } },
    { "<leader>aC", "", desc = "ClaudeCode", mode = { "n", "v" } },
    { "<leader>aCc", "<cmd>ClaudeCode<cr>", desc = "Toggle Claude" },
    { "<leader>aCf", "<cmd>ClaudeCodeFocus<cr>", desc = "Focus Claude" },
    { "<leader>aCr", "<cmd>ClaudeCode --resume<cr>", desc = "Resume Claude" },
    { "<leader>aCC", "<cmd>ClaudeCode --continue<cr>", desc = "Continue Claude" },
    { "<leader>aCb", "<cmd>ClaudeCodeAdd %<cr>", desc = "Add current buffer" },
    { "<leader>aCs", "<cmd>ClaudeCodeSend<cr>", mode = "v", desc = "Send to Claude" },
    {
      "<leader>aCs",
      "<cmd>ClaudeCodeTreeAdd<cr>",
      desc = "Add file",
      ft = { "NvimTree", "neo-tree", "oil" },
    },
    -- Diff management
    { "<leader>aCa", "<cmd>ClaudeCodeDiffAccept<cr>", desc = "Accept diff" },
    { "<leader>aCd", "<cmd>ClaudeCodeDiffDeny<cr>", desc = "Deny diff" },
  },
}
