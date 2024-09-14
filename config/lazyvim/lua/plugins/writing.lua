local _, openai_api_key = RunShellCommand("copy_password.sh apikey")

return {
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
		config = function()
			vim.cmd([[
        augroup pencil
          autocmd!
          autocmd FileType markdown,mkd call pencil#init()
          autocmd FileType texfile      call pencil#init()
          autocmd FileType text         call pencil#init()
        augroup END
      ]])
		end,
	},
	-- correct common typos and misspellings as you type in vim
	{
		"panozzaj/vim-autocorrect",
		config = function()
			vim.cmd([[
        augroup autocorrect
          autocmd!
          autocmd FileType markdown,mkd call AutoCorrect()
          autocmd FileType text         call AutoCorrect()
          autocmd FileType texfile      call AutoCorrect()
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
		"williamboman/mason.nvim",
		opts = function(_, opts)
			table.insert(opts.ensure_installed, "textlsp")
		end,
	},
	-- text lsp server for grammar checking
	{
		"neovim/nvim-lspconfig",
		opts = {
			servers = {
				textlsp = {
					filetypes = { "tex", "latex", "pandoc", "markdown" },
					settings = {
						textLSP = {
							analysers = {
								languagetool = {
									enabled = false,
								},
								gramformer = {
									-- gramformer dependency needs to be installed manually
									enabled = true,
									gpu = true,
									check_text = {
										on_open = false,
										on_save = false,
										on_change = false,
									},
								},
								openai = {
									enabled = true,
									api_key = openai_api_key,
									check_text = {
										on_open = false,
										on_save = false,
										on_change = false,
									},
									model = "gpt-4o-mini",
									max_token = 50,
								},
							},
						},
					},
				},
			},
		},
	},
}
