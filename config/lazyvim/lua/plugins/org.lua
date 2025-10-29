return {
  {
    "nvim-orgmode/orgmode",
    dependencies = {
      "nvim-telescope/telescope.nvim",
      "nvim-orgmode/telescope-orgmode.nvim",
      "nvim-orgmode/org-bullets.nvim",
      "danilshvalov/org-modern.nvim",
      "Saghen/blink.cmp",
    },
    event = "VeryLazy",
    ft = { "org" },
    config = function()
      local Menu = require("org-modern.menu")

      require("orgmode").setup({

        ui = {
          menu = {
            handler = function(data)
              Menu:new():open(data)
            end,
          },
        },
        org_agenda_files = {
          "~/Dropbox/Org/inbox.org",
          "~/Dropbox/Org/someday.org",
          "~/Dropbox/Org/work.org",
          "~/Dropbox/Org/personal.org",
          "~/Dropbox/Org/ideas.org",
          "~/Dropbox/Org/projects/**",
        },
        org_default_notes_file = "~/Dropbox/Org/refile.org",
      })
      require("org-bullets").setup()
      require("blink.cmp").setup({
        sources = {
          per_filetype = {
            org = { "orgmode" },
          },
          providers = {
            orgmode = {
              name = "Orgmode",
              module = "orgmode.org.autocompletion.blink",
              fallbacks = { "buffer" },
            },
          },
        },
      })

      require("telescope").setup()
      require("telescope").load_extension("orgmode")
      vim.keymap.set("n", "<leader>r", require("telescope").extensions.orgmode.refile_heading)
      vim.keymap.set("n", "<leader>fh", require("telescope").extensions.orgmode.search_headings)
      vim.keymap.set("n", "<leader>li", require("telescope").extensions.orgmode.insert_link)
    end,
  },
  {
    "chipsenkbeil/org-roam.nvim",
    tag = "0.2.0",
    dependencies = {
      {
        "nvim-orgmode/orgmode",
        tag = "0.7.0",
      },
    },
    event = "VeryLazy",
    ft = { "org" },
    config = function()
      require("org-roam").setup({
        directory = "~/Dropbox/Org/",
      })
    end,
  },
  {
    "hamidi-dev/org-super-agenda.nvim",
    dependencies = {
      "nvim-orgmode/orgmode", -- required
      { "lukas-reineke/headlines.nvim", config = true }, -- optional nicety
    },

    config = function()
      require("org-super-agenda").setup({
        org_files = {
          "~/Dropbox/Org/inbox.org",
          "~/Dropbox/Org/someday.org",
          "~/Dropbox/Org/work.org",
          "~/Dropbox/Org/personal.org",
          "~/Dropbox/Org/ideas.org",
          "~/Dropbox/Org/projects/**",
        },
        org_directories = { "~/Dropbox/Org/" },
      })
    end,
  },
  {
    "bi0ha2ard/telescope-org_roam.nvim",
    requires = {
      { "nvim-telescope/telescope.nvim" },
    },
  },
}
