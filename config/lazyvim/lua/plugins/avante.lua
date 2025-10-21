return {
  { "MunifTanjim/nui.nvim", lazy = true },

  {
    "yetone/avante.nvim",
    build = vim.fn.has("win32") ~= 0 and "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false"
      or "make",
    event = "VeryLazy",
    opts = {
      provider = "copilot",
      selection = {
        hint_display = "none",
      },
      behaviour = {
        auto_set_keymaps = false,
      },
    },
    cmd = {
      "AvanteAsk",
      "AvanteBuild",
      "AvanteChat",
      "AvanteClear",
      "AvanteEdit",
      "AvanteFocus",
      "AvanteHistory",
      "AvanteModels",
      "AvanteRefresh",
      "AvanteShowRepoMap",
      "AvanteStop",
      "AvanteSwitchProvider",
      "AvanteToggle",
    },
    keys = {
      { "<leader>aa", "", desc = "Avante", mode = { "n", "v" } },
      { "<leader>aaa", "<cmd>AvanteAsk<CR>", desc = "Ask Avante" },
      { "<leader>aac", "<cmd>AvanteChat<CR>", desc = "Chat with Avante" },
      { "<leader>aae", "<cmd>AvanteEdit<CR>", desc = "Edit Avante" },
      { "<leader>aaf", "<cmd>AvanteFocus<CR>", desc = "Focus Avante" },
      { "<leader>aah", "<cmd>AvanteHistory<CR>", desc = "Avante History" },
      { "<leader>aam", "<cmd>AvanteModels<CR>", desc = "Select Avante Model" },
      { "<leader>aan", "<cmd>AvanteChatNew<CR>", desc = "New Avante Chat" },
      { "<leader>aap", "<cmd>AvanteSwitchProvider<CR>", desc = "Switch Avante Provider" },
      { "<leader>aar", "<cmd>AvanteRefresh<CR>", desc = "Refresh Avante" },
      { "<leader>aas", "<cmd>AvanteStop<CR>", desc = "Stop Avante" },
      { "<leader>aat", "<cmd>AvanteToggle<CR>", desc = "Toggle Avante" },
    },
  },

  -- support for image pasting
  {
    "HakonHarnes/img-clip.nvim",
    event = "VeryLazy",
    optional = true,
    opts = {
      -- recommended settings
      default = {
        embed_image_as_base64 = false,
        prompt_for_file_name = false,
        drag_and_drop = {
          insert_mode = true,
        },
        -- required for Windows users
        use_absolute_path = true,
      },
    },
  },

  -- Make sure to set this up properly if you have lazy=true
  {
    "MeanderingProgrammer/render-markdown.nvim",
    optional = true,
    opts = {
      file_types = { "markdown", "Avante" },
    },
    ft = { "markdown", "Avante" },
  },

  -- blink.cmp source for avante.nvim
  {
    "saghen/blink.cmp",
    optional = true,
    specs = { "Kaiser-Yang/blink-cmp-avante" },
    opts = {
      sources = {
        default = { "avante" },
        providers = { avante = { module = "blink-cmp-avante", name = "Avante" } },
      },
    },
  },
}
