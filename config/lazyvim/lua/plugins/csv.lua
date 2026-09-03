return {
  "mechatroner/rainbow_csv",
  -- Neovim's builtin ftdetect already maps .csv/.tsv to these filetypes,
  -- which rainbow_csv's own detection logic (autoload/rainbow_csv.vim)
  -- special-cases too.
  ft = { "csv", "tsv" },
}
