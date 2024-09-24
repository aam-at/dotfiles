vim.g.is_goyo_active = false

vim.g.goyo_enter = function()
	if vim.fn.executable("tmux") == 1 and vim.fn.strlen(vim.env.TMUX) > 0 then
		vim.fn.system("tmux set status off")
		if not string.find(vim.fn.system("tmux list-panes -F '#F'"), "Z") then
			vim.fn.system("tmux resize-pane -Z")
		end
	end

	vim.g.default_colorscheme = vim.g.colors_name or "default"
	vim.o.statusline = ""
	vim.wo.linebreak = true
	vim.wo.wrap = true
	vim.bo.textwidth = 0
	vim.bo.wrapmargin = 0
	vim.cmd("Goyo 140x95%")
	vim.cmd("Limelight 0.5")
	require("lualine").hide()
	vim.g.is_goyo_active = true
end

vim.g.goyo_leave = function()
	if vim.fn.executable("tmux") == 1 and vim.fn.strlen(vim.env.TMUX) > 0 then
		vim.fn.system("tmux set status on")
		if string.find(vim.fn.system("tmux list-panes -F '#F'"), "Z") then
			vim.fn.system("tmux resize-pane -Z")
		end
	end
	vim.cmd("Goyo!")
	vim.cmd("Limelight!")
	vim.cmd("colorscheme " .. vim.g.default_colorscheme)
	vim.g.is_goyo_active = false
end

vim.g.toggle_goyo = function()
	if vim.g.is_goyo_active then
		vim.g.goyo_leave()
	else
		vim.g.goyo_enter()
	end
end

return {
	-- goyo.vim - Distraction-free writing in Vim
	{
		"junegunn/goyo.vim",
		depends = { "junegunn/limelight.vim" },
		cmd = "Goyo",
		keys = {
			vim.api.nvim_set_keymap(
				"n",
				"<leader>uG",
				"<cmd>lua vim.g.toggle_goyo()<CR>",
				{ noremap = true, silent = true, desc = "Toggle Goyo" }
			),
		},
	},
	-- limelight - Hyperfocus-writing in Vim
	{
		"junegunn/limelight.vim",
		cmd = "Limelight",
	},
}
