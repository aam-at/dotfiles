return {
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim", -- required
      "sindrets/diffview.nvim", -- optional - Diff integration
    },
    config = true,
    keys = {
      { "<leader>gm", "<cmd>Neogit<CR>", desc = "Neogit Status" },
    },
  },
}
