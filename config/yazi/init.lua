local gruvbox_theme = require("yatline-gruvbox"):setup("dark")

-- Plugins
require("full-border"):setup({
	type = ui.Border.ROUNDED,
})

require("zoxide"):setup({
	update_db = true,
})

require("session"):setup({
	sync_yanked = true,
})

require("yatline"):setup({
	theme = gruvbox_theme,

	tab_width = 20,
	tab_use_inverse = true,

	show_background = true,

	display_header_line = true,
	display_status_line = true,

	header_line = {
		left = {
			section_a = {
				{ type = "line", custom = false, name = "tabs", params = { "left" } },
			},
			section_b = {},
			section_c = {},
		},
		right = {
			section_a = {
				{ type = "string", custom = false, name = "tab_path" },
			},
			section_b = {
				{ type = "coloreds", custom = false, name = "githead" },
			},
			section_c = {},
		},
	},

	status_line = {
		left = {
			section_a = {
				{ type = "string", custom = false, name = "tab_mode" },
			},
			section_b = {
				{ type = "string", custom = false, name = "hovered_size" },
			},
			section_c = {
				{ type = "string", custom = false, name = "hovered_name" },
				{ type = "coloreds", custom = false, name = "count" },
			},
		},
		right = {
			section_a = {
				{ type = "string", custom = false, name = "cursor_position" },
			},
			section_b = {
				{ type = "string", custom = false, name = "cursor_percentage" },
			},
			section_c = {
				{ type = "string", custom = false, name = "hovered_file_extension", params = { true } },
				{ type = "coloreds", custom = false, name = "permissions" },
				{ type = "coloreds", custom = false, name = "created_time" },
				{ type = "coloreds", custom = false, name = "modified_time" },
			},
		},
	},
})

require("yatline-modified-time"):setup()

require("yatline-created-time"):setup()

require("yatline-githead"):setup({
	show_branch = true,
	branch_prefix = "",
	branch_symbol = "",
	branch_borders = "",

	commit_symbol = " ",

	show_behind_ahead = true,
	behind_symbol = " ",
	ahead_symbol = " ",

	show_stashes = true,
	stashes_symbol = " ",

	show_state = true,
	show_state_prefix = true,
	state_symbol = "󱅉",

	show_staged = true,
	staged_symbol = " ",

	show_unstaged = true,
	unstaged_symbol = " ",

	show_untracked = true,
	untracked_symbol = " ",

	prefix_color = gruvbox_theme.pink,
	branch_color = gruvbox_theme.pink,
	commit_color = gruvbox_theme.mauve,
	stashes_color = gruvbox_theme.teal,
	state_color = gruvbox_theme.lavender,
	staged_color = gruvbox_theme.green,
	unstaged_color = gruvbox_theme.yellow,
	untracked_color = gruvbox_theme.pink,
	ahead_color = gruvbox_theme.green,
	behind_color = gruvbox_theme.yellow,
})

require("git"):setup()
