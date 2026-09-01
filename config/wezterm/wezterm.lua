-- Initialize Configuration
local wezterm = require("wezterm")
local config = wezterm.config_builder()
-- Matches kitty's background_opacity 0.95: true window translucency, letting
-- niri's compositor blur the wallpaper through it, rather than a static
-- baked "blurred" image (config/wezterm/bg-blurred.png was that image; it
-- was corrupted by a .gitattributes eol=lf rule mangling its binary bytes,
-- with no clean copy left in git history to restore, so it's gone — see
-- .gitattributes for the fix that stops this from recurring).
local opacity = 0.95
local transparent_bg = "rgba(22, 24, 26, " .. opacity .. ")"

--- Get the current operating system
--- @return "windows"| "linux" | "macos"
local function get_os()
    local bin_format = package.cpath:match("%p[\\|/]?%p(%a+)")
    if bin_format == "dll" then
        return "windows"
    elseif bin_format == "so" then
        return "linux"
    end

    return "macos"
end

local host_os = get_os()

-- Font Configuration
local emoji_font = "Segoe UI Emoji"
if host_os == "linux" then
    emoji_font = "Noto Color Emoji"
end
config.font = wezterm.font_with_fallback({
    {
        family = "JetBrainsMono Nerd Font",
        weight = "Regular",
    },
    emoji_font,
})
config.font_size = 10

-- Scrollback
config.scrollback_lines = 10000

-- Color Configuration. Keep this adapter in the centralized theme folder.
config.colors = dofile(os.getenv("HOME") .. "/.config/theme/wezterm.lua")
config.force_reverse_video_cursor = true

-- Window Configuration
config.initial_rows = 45
config.initial_cols = 180
config.window_decorations = "RESIZE"
config.window_background_opacity = opacity
config.window_close_confirmation = "NeverPrompt"
config.win32_system_backdrop = "Acrylic"

-- Performance Settings
config.max_fps = 144
config.animation_fps = 60
config.cursor_blink_rate = 250

-- Tab Bar Configuration
config.enable_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true
config.show_tab_index_in_tab_bar = false
config.use_fancy_tab_bar = false
config.colors.tab_bar = {
    background = transparent_bg,
    new_tab = { fg_color = config.colors.background, bg_color = config.colors.brights[6] },
    new_tab_hover = { fg_color = config.colors.background, bg_color = config.colors.foreground },
}

-- Tab Formatting
wezterm.on("format-tab-title", function(tab, _, _, _, hover)
    local background = config.colors.brights[1]
    local foreground = config.colors.foreground

    if tab.is_active then
        background = config.colors.brights[7]
        foreground = config.colors.background
    elseif hover then
        background = config.colors.brights[8]
        foreground = config.colors.background
    end

    local title = tostring(tab.tab_index + 1)
    return {
        { Foreground = { Color = background } },
        { Text = "█" },
        { Background = { Color = background } },
        { Foreground = { Color = foreground } },
        { Text = title },
        { Foreground = { Color = background } },
        { Text = "█" },
    }
end)

-- Keybindings
config.keys = {
    { key = "v", mods = "CTRL", action = wezterm.action({ PasteFrom = "Clipboard" }) },
}

-- Default Shell Configuration
config.default_prog = { "pwsh", "-NoLogo" }

-- OS-Specific Overrides
if host_os == "linux" then
    config.default_prog = { "fish" }
    config.front_end = "WebGpu"
    config.wayland_window_background_blur = true -- compositor-blurred wallpaper under the translucent window (niri)
    config.window_decorations = nil -- use system decorations
end

return config
