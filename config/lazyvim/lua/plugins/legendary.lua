return {
  {
    "mrjones2014/legendary.nvim",
    priority = 10000,
    cmd = "Legendary",
    dependencies = { "kkharji/sqlite.lua" },
    config = function()
      require("legendary").setup({
        extensions = {
          diffview = true,
          lazy_nvim = true,
        },
      })
    end,
  },
}
