return {
  {
    "mason-org/mason.nvim",
    opts = function(_, opts)
      table.insert(opts.ensure_installed, "ltex-ls-plus")
      table.insert(opts.ensure_installed, "texlab")
      table.insert(opts.ensure_installed, "vale")
    end,
  },
  {
    "lervag/vimtex",
    lazy = false, -- lazy-loading will disable inverse search
    config = function()
      vim.api.nvim_create_autocmd({ "FileType" }, {
        group = vim.api.nvim_create_augroup("lazyvim_vimtex_conceal", { clear = true }),
        pattern = { "bib", "tex" },
        callback = function()
          vim.wo.conceallevel = 0
        end,
      })

      vim.g.vimtex_view_method = "zathura"
      vim.g.vimtex_view_method_sync = 1
      vim.g.vimtex_view_method_activate = 1
      vim.g.vimtex_view_method_reading_bar = 1
      vim.g.vimtex_toc_config = {
        name = "LaTeX TOC",
        show_help = 0,
        show_numbers = 1,
        mode = 2,
      }

      vim.g.vimtex_compiler_latexmk = {
        aux_dir = "./aux",
        out_dir = "./out",
      }
    end,
    keys = {
      { "<localleader>l", "", desc = "+LaTeX", ft = "tex" },
      { "<localleader>ll", "<cmd>VimtexCompile<cr>", desc = "Compile", ft = "tex" },
      { "<localleader>lv", "<cmd>VimtexView<cr>", desc = "View PDF", ft = "tex" },
      { "<localleader>le", "<cmd>VimtexErrors<cr>", desc = "Show errors", ft = "tex" },
      { "<localleader>lc", "<cmd>VimtexClean<cr>", desc = "Clean build files", ft = "tex" },
      { "<localleader>lt", "<cmd>VimtexTocToggle<cr>", desc = "Toggle outline", ft = "tex" },
      { "<localleader>lm", "<cmd>VimtexToggleMain<cr>", desc = "Toggle main file", ft = "tex" },
    },
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        ltex_plus = {
          settings = {
            ltex = {
              language = "en-US",
            },
          },
        },
        texlab = {
          settings = {
            texlab = {
              build = {
                executable = "latexmk",
                args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "-file-line-error", "%f" },
                onSave = false,
                forwardSearchAfter = false,
              },
              latexFormatter = "latexindent",
              latexindent = { modifyLineBreaks = false },
              symbols = {
                customEnvironments = {
                  { name = "frame", displayName = "Frame", label = false },
                  { name = "block", displayName = "Block", label = false },
                  { name = "alertblock", displayName = "Alert block", label = false },
                  { name = "exampleblock", displayName = "Example block", label = false },
                },
              },
            },
          },
        },
      },
    },
  },
  {
    "L3MON4D3/LuaSnip",
    ft = { "tex", "plaintex" },
    dependencies = { "iurimateus/luasnip-latex-snippets.nvim" },
    config = function()
      require("luasnip").config.setup({
        enable_autosnippets = true,
        updateevents = "TextChanged,TextChangedI",
      })
      require("luasnip-latex-snippets").setup()
    end,
  },
  {
    "aam-at/texpresso.vim",
    dependencies = {
      { "lervag/vimtex" },
    },
    commands = {
      "TeXpresso",
    },
    config = function()
      vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
        group = vim.api.nvim_create_augroup("lazyvim_texpresso_keymap", { clear = true }),
        pattern = { "*.tex" },
        callback = function()
          vim.api.nvim_buf_set_keymap(0, "n", "<leader>cx", ":TeXpresso %<CR>", { noremap = true, silent = true })
        end,
      })
    end,
  },
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      { "kdheepak/cmp-latex-symbols" },
    },
    opts = function(_, opts)
      table.insert(opts.sources, {
        name = "latex_symbols",
      })
    end,
  },
  {
    "saghen/blink.cmp",
    optional = true,
    dependencies = { "kdheepak/cmp-latex-symbols", "saghen/blink.compat" },
    opts = {
      sources = {
        compat = { "latex_symbols" },
        providers = {
          latex_symbols = {
            kind = "LatexSymbols",
            async = true,
            opts = { strategy = 0 }, -- mixed command and symbol matching
          },
        },
      },
    },
  },
  {
    "nvim-telescope/telescope-bibtex.nvim",
    dependencies = {
      { "nvim-telescope/telescope.nvim" },
    },
    cmd = "Telescope",
    keys = {
      { "<leader>fb", "<cmd>Telescope bibtex<cr>", desc = "Search bibliography" },
    },
    config = function()
      require("telescope").load_extension("bibtex")
      require("telescope").setup({
        extensions = {
          bibtex = {
            global_files = {
              "~/Dropbox/Research/Bibliography/refs.bib",
              "~/Dropbox/Research/Bibliography/myrefs.bib",
            },
          },
        },
      })
    end,
  },
  {
    "mfussenegger/nvim-lint",
    optional = true,
    opts = {
      linters_by_ft = {
        org = { "vale" },
        txt = { "vale" },
        latex = { "vale" },
        markdown = { "vale" },
      },
    },
  },
}
