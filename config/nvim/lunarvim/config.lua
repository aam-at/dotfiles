-- Read the docs: https://www.lunarvim.org/docs/configuration
-- Example configs: https://github.com/LunarVim/starter.lvim
-- Video Tutorials: https://www.youtube.com/watch?v=sFA9kX-Ud_c&list=PLhoH5vyxr6QqGu0i7tt_XoVK9v-KvZ3m6
-- Forum: https://www.reddit.com/r/lunarvim/
-- Discord: https://discord.com/invite/Xb9B4Ny

lvim.plugins = {
  -- Gruvbox colorscheme
  {
    "ellisonleao/gruvbox.nvim",
    config = true
  },
  -- Telescope extensions
  {
    "nvim-telescope/telescope-fzy-native.nvim",
    build = "make",
    event = "BufRead",
  },
  {
    "nvim-telescope/telescope-project.nvim",
    event = "BufWinEnter",
  },
  -- Copilot integration
  {
    "zbirenbaum/copilot-cmp",
    event = "InsertEnter",
    dependencies = { "zbirenbaum/copilot.lua" },
    config = function()
      vim.defer_fn(function()
        require("copilot").setup() -- https://github.com/zbirenbaum/copilot.lua/blob/master/README.md#setup-and-configuration
        require("copilot_cmp").setup() -- https://github.com/zbirenbaum/copilot-cmp/blob/master/README.md#configuration
      end, 100)
    end,
  },
  -- CodeCompanion setup
  {
    "olimorris/codecompanion.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      "hrsh7th/nvim-cmp", -- Optional: For using slash commands and variables in the chat buffer
      "nvim-telescope/telescope.nvim", -- Optional: For working with files with slash commands
      {
        "stevearc/dressing.nvim", -- Optional: Improves the default Neovim UI
        opts = {},
      },
    },
    config = function()
      vim.defer_fn(function()
        require("codecompanion").setup(
          {
            adapters = {
              openai = function()
                return require("codecompanion.adapters").extend("openai", {
                  env = {
                    api_key = "OPENAI_API_KEY",
                  },
                })
              end,
            },
            strategies = {
              chat = {
                adapter = "copilot",
              },
              inline = {
                adapter = "copilot",
              },
              agent = {
                adapter = "copilot",
              },
            },
            opts = {
              log_level = "TRACE",
            }
          })
      end, 100)
    end,
  }
}

-- Telescope configuration
lvim.builtin.telescope.on_config_done = function(telescope)
  pcall(telescope.load_extension, "frecency")
  pcall(telescope.load_extension, "neoclip")
  pcall(telescope.load_extension, "project")
  -- any other extensions loading
end

-- Set colorscheme
lvim.colorscheme = "gruvbox"
lvim.dark = true
