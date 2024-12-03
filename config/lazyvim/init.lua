-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")

LazyVim.on_load("telescope", function(telescope)
	pcall(telescope.load_extension, "frecency")
	pcall(telescope.load_extension, "fzf")
	pcall(telescope.load_extension, "neoclip")
	pcall(telescope.load_extension, "project")
	pcall(telescope.load_extension, "org_roam")
	pcall(function()
		telescope.load_extension("bibtex")
		require("telescope").extensions.bibtex.setup({
			-- Path to global bibliographies (placed outside of the project)
			global_files = {
				"~/Google Drive/Research/Bibliography/refs.bib",
				"~/Google Drive/Research/Bibliography/books.bib",
				"~/Google Drive/Research/Bibliography/myrefs.bib",
			},
			-- Context awareness
			context = true,
		})
	end)
end)

vim.o.background = "dark" -- or "light" for light mode
vim.cmd([[colorscheme gruvbox]])
vim.o.guifont = "JetBrains Mono:h12"
