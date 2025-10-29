return {
  {
    "nvimdev/lspsaga.nvim",
    after = "nvim-lspconfig",
    config = function()
      require("lspsaga").setup({})
    end,
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        -- apply to all servers with ['*']
        ["*"] = {
          keys = {
            { "K", "<cmd>Lspsaga hover_doc<cr>", desc = "Show doc", has = "hover" },
            { "gd", "<cmd>Lspsaga goto_definition<cr>", desc = "Goto Definition", has = "definition" },
            { "gD", "<cmd>Lspsaga goto_type_definition<cr>", desc = "Goto Declaration", has = "typeDefinition" },
            { "gp", "<cmd>Lspsaga peek_definition<cr>", desc = "Peek Definition", has = "definition" },
            { "gP", "<cmd>Lspsaga peek_type_definition<cr>", desc = "Peek Declaration", has = "typeDefinition" },
            { "gr", "<cmd>Lspsaga finder<cr>", desc = "References", has = "references" },
            {
              "gR",
              "<cmd>Telescope lsp_references<cr>",
              desc = "References (Telescope)",
              nowait = true,
              has = "references",
            },
            { "<leader>cO", "<cmd>Lspsaga outline<cr>", desc = "Toggle outline" },
            { "<leader>'", "<cmd>Lspsaga term_toggle<cr>", desc = "Term toggle" },
          },
        },
      },
    },
  },
}
