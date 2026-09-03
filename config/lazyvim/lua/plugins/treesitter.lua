return {
  -- Build tool for treesitter parsers with no prebuilt binary
  {
    "mason-org/mason.nvim",
    opts = function(_, opts)
      table.insert(opts.ensure_installed, "tree-sitter-cli")
    end,
  },
}
