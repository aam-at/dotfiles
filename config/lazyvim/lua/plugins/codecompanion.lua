return {
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
    keys = {
      { "<leader>a", "", desc = "+ai", mode = { "n", "v" } },
      { "<leader>ao", "", desc = "CodeCompanion", mode = { "n", "v" } },
      { "<leader>aoc", "", desc = "CodeCompanion Chat", mode = { "n", "v" } },
      {
        "<leader>aoca",
        function()
          return require("codecompanion").chat({ fargs = { "anthropic" } })
        end,
        desc = "CodeCompanion Chat with Anthropic",
        mode = { "n", "v" },
      },
      {
        "<leader>aocc",
        function()
          return require("codecompanion").chat({ fargs = { "copilot" } })
        end,
        desc = "CodeCompanion Chat with Copilot",
        mode = { "n", "v" },
      },
      {
        "<leader>aoco",
        function()
          return require("codecompanion").chat({ fargs = { "openai" } })
        end,
        desc = "CodeCompanion Chat with OpenAI",
        mode = { "n", "v" },
      },
      {
        "<leader>aocg",
        function()
          return require("codecompanion").chat({ fargs = { "gemini" } })
        end,
        desc = "CodeCompanion Chat with Gemini",
        mode = { "n", "v" },
      },
      {
        "<leader>aocd",
        function()
          return require("codecompanion").chat({ fargs = { "deepseek" } })
        end,
        "<cmd>CodeCompanionChat deepseek<CR>",
        desc = "CodeCompanion Chat with DeepSeek",
        mode = { "n", "v" },
      },
      {
        "<leader>aom",
        function()
          return require("codecompanion").actions()
        end,
        desc = "CodeCompanion Actions",
        mode = { "n", "v" },
      },
      {
        "<leader>aot",
        function()
          return require("codecompanion").toggle()
        end,
        desc = "CodeCompanion Toggle",
        mode = { "n", "v" },
      },
      {
        "<leader>aoa",
        function()
          return require("codecompanion").add()
        end,
        desc = "CodeCompanion Add",
        mode = { "v" },
      },
    },
    config = function()
      vim.defer_fn(function()
        require("codecompanion").setup({
          adapters = {
            openai = function()
              return require("codecompanion.adapters").extend("openai", {
                env = {
                  api_key = "cmd:copy_password.sh apikey",
                },
              })
            end,
            gemini = function()
              return require("codecompanion.adapters").extend("gemini", {
                env = {
                  api_key = "cmd:copy_password.sh gemini",
                },
              })
            end,
            deepseek = function()
              return require("codecompanion.adapters").extend("openai", {
                url = "https://api.deepseek.com/v1/chat/completions",
                env = {
                  api_key = "cmd:copy_password.sh deepseek",
                },
                schema = {
                  model = {
                    order = 1,
                    mapping = "parameters",
                    type = "enum",
                    default = "deepseek-chat",
                    choices = {
                      "deepseek-chat",
                      "deepseek-coder",
                    },
                  },
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
          },
        })
      end, 100)
    end,
  },
  -- Blink completion
  {
    "saghen/blink.cmp",
    version = "*",
    opts = {
      sources = {
        -- Change the sources to suit your config
        default = { "lsp", "path", "buffer", "codecompanion" },
        providers = {
          codecompanion = {
            name = "CodeCompanion",
            module = "codecompanion.providers.completion.blink",
          },
        },
      },
    },
  },
  -- Markdown rendering
  { "MeanderingProgrammer/render-markdown.nvim", ft = { "markdown", "codecompanion" } },
  -- Edgy integration
  {
    "folke/edgy.nvim",
    optional = true,
    opts = function(_, opts)
      opts.right = opts.right or {}
      table.insert(opts.right, {
        ft = "codecompanion",
        title = "CodeCompanion Chat",
        size = { width = 80 },
      })
    end,
  },
}
