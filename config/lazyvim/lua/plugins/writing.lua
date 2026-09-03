return {
  -- line wrapping
  {
    "andrewferrier/wrapping.nvim",
    ft = { "markdown", "mkd", "text", "tex" },
    config = function()
      require("wrapping").setup()
    end,
  },
  -- uncover usage problems in your writing
  {
    "reedes/vim-wordy",
    ft = { "txt", "tex", "org" },
  },
  -- lightweight auto-correction for Vim
  {
    "reedes/vim-litecorrect",
    config = function()
      vim.cmd([[
        augroup litecorrect
          autocmd!
          autocmd FileType markdown,mkd call litecorrect#init()
          autocmd FileType textile      call litecorrect#init()
          autocmd FileType text         call litecorrect#init()
        augroup END
      ]])
    end,
  },
  -- rethinking Vim as a tool for writers
  {
    "reedes/vim-pencil",
    cmd = "PencilToggle",
    ft = { "markdown", "mkd", "text", "tex" },
    config = function()
      vim.cmd([[
        augroup pencil
          autocmd!
          autocmd FileType markdown,mkd call pencil#init()
          autocmd FileType tex          call pencil#init()
          autocmd FileType text         call pencil#init()
        augroup END
      ]])
    end,
  },
  -- correct common typos and misspellings as you type in vim
  {
    "panozzaj/vim-autocorrect",
    ft = { "markdown", "mkd", "text", "tex" },
    config = function()
      vim.cmd([[
        augroup autocorrect
          autocmd!
          autocmd FileType markdown,mkd call AutoCorrect()
          autocmd FileType text         call AutoCorrect()
          autocmd FileType tex          call AutoCorrect()
        augroup END
      ]])
    end,
  },
  -- stop repeating yourself
  {
    "dbmrq/vim-ditto",
    cmd = "ToggleDitto",
  },
  -- automatically install textlsp
  {
    "mason-org/mason.nvim",
    opts = function(_, opts)
      table.insert(opts.ensure_installed, "textlsp")
    end,
  },
}
