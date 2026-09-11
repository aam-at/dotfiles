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

-- yatline (rev c5d4b48) still calls the `File:icon()` API that yazi 26.8 deprecated
-- in favour of `th.icon:match(file)`. Override the one component that hits it,
-- rather than patching the vendored plugin (which `ya pkg` upgrades would clobber).
function Yatline.string.get:hovered_file_extension(show_icon)
  local hovered = cx.active.current.hovered
  if not hovered then
    return ""
  end

  local name
  if hovered.cha.is_dir then
    name = "dir"
  else
    name = hovered.url.name:match("^.+%.(.+)$") or "null"
  end

  if not show_icon then
    return name
  end

  local icon
  if th.icon then
    icon = th.icon:match(hovered)
  else -- yazi < 25.x fallback
    icon = hovered:icon()
  end

  return (icon and icon.text .. " " or "") .. name
end

-- yatline (rev c5d4b48) still reads the deprecated Url.is_search API.
-- Override the active component with Yazi's Url.spec.is_search API.
function Yatline.string.get:tab_path(trimmed, max_length, trim_length)
  trimmed = trimmed or false
  max_length = max_length or 24
  trim_length = trim_length or 10

  local cwd = cx.active.current.cwd
  local filter = cx.active.current.files.filter
  local finder = cx.active.finder
  local spec = cwd.spec
  local is_search, domain
  if spec then
    is_search = spec.is_search
    domain = spec.domain
  else -- yazi < 26.x fallback
    is_search = cwd.is_search
    domain = cwd.domain
  end

  local parts = {}
  if is_search then
    parts[#parts + 1] = string.format("search: %s", domain)
  end
  if filter then
    parts[#parts + 1] = string.format("filter: %s", filter)
  end
  if finder then
    parts[#parts + 1] = string.format("find: %s", finder)
  end

  local suffix = #parts > 0 and " (" .. table.concat(parts, ", ") .. ")" or ""
  local path = ya.readable_path(tostring(cwd))
  if trimmed then
    path = ui.truncate(path, { max = max_length })
  end
  return path .. suffix
end

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
