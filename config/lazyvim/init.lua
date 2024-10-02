-- my custom commands
function RunShellCommand(cmd)
	local handle, err = io.popen(cmd)
	if not handle then
		return false, err
	end

	local result, read_err = handle:read("*a")
	local _, close_err = handle:close()

	if read_err or close_err then
		return false, read_err or close_err
	end

	result = string.gsub(result, "[\r\n]+$", "")
	return true, result
end

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
				"~/Dropbox/Research/Bibliography/refs.bib",
				"~/Dropbox/Research/Bibliography/myrefs.bib",
			},
			-- Context awareness
			context = true,
		})
	end)
end)

vim.o.background = "dark" -- or "light" for light mode
vim.cmd([[colorscheme gruvbox]])
vim.o.guifont = "JetBrains Mono:h12"