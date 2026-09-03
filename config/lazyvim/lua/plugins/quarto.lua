-- Quarto (.qmd) editing, adapted from
-- https://github.com/jmbuhr/nvim-config/blob/main/lua/plugins/quarto.lua
-- Deliberately drops jpalardy/vim-slime and benlubas/molten-nvim (REPL /
-- notebook code execution) — this config only covers editing/LSP features.
return {
  {
    "quarto-dev/quarto-nvim",
    ft = { "quarto" },
    dependencies = {
      -- Provides LSP features and treesitter language injection inside code cells
      "jmbuhr/otter.nvim",
    },
    opts = {
      lspFeatures = {
        enabled = true,
        chunks = "curly",
      },
    },
  },

  -- Embedded-language LSP/completion/diagnostics inside code cells; loads as
  -- a dependency of quarto-nvim above, this just supplies its setup() call.
  {
    "jmbuhr/otter.nvim",
    opts = {},
  },

  -- Parsers needed to detect and highlight code-cell languages inside .qmd
  -- files (markdown/markdown_inline/python are already in LazyVim's defaults).
  -- "latex" has no prebuilt binary and needs tree-sitter-cli to compile;
  -- that Mason install lives in treesitter.lua, not here.
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = { "r", "julia", "latex" },
    },
  },

  -- Open .ipynb files directly as Quarto documents, converting back behind
  -- the scenes. The plugin's own docs recommend against lazy-loading it.
  {
    "GCBallesteros/jupytext.nvim",
    opts = {
      custom_language_formatting = {
        python = {
          extension = "qmd",
          style = "quarto",
          force_ft = "quarto",
        },
        r = {
          extension = "qmd",
          style = "quarto",
          force_ft = "quarto",
        },
      },
    },
  },

  -- Paste an image from the clipboard or drag-and-drop
  {
    "HakonHarnes/img-clip.nvim",
    event = "BufEnter",
    ft = { "markdown", "quarto", "latex" },
    opts = {
      default = {
        dir_path = "img",
        drag_and_drop = {
          enabled = false,
          insert_mode = false,
        },
      },
      filetypes = {
        markdown = {
          url_encode_path = true,
          template = "![$CURSOR]($FILE_PATH)",
          drag_and_drop = {
            download_images = false,
          },
        },
        quarto = {
          url_encode_path = true,
          template = "![$CURSOR]($FILE_PATH)",
          drag_and_drop = {
            download_images = false,
          },
        },
      },
    },
    config = function(_, opts)
      require("img-clip").setup(opts)
      vim.keymap.set("n", "<leader>ii", ":PasteImage<cr>", { desc = "Insert image from clipboard" })
    end,
  },

  -- Preview LaTeX-style math equations inline
  {
    "jbyuki/nabla.nvim",
    keys = {
      -- <leader>u* is LazyVim's UI-toggle group (us Spelling, uw Wrap, ...);
      -- these are toggle/preview actions, so they belong there rather than
      -- under <leader>q (Quit) or a bare, groupless <leader>p.
      {
        "<leader>um",
        function()
          require("nabla").toggle_virt()
        end,
        desc = "Toggle Math Equations",
      },
      {
        "<leader>uM",
        function()
          require("nabla").popup()
        end,
        desc = "Preview Math Equation (popup)",
      },
    },
  },
}
