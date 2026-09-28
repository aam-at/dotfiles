-- Today's screen time from dotfiles' tools/wellbeing (`wellbeing --status`,
-- the same JSON windots' YASB widget shows): an hourglass, or a moon while
-- focus mode is on. Click shows the top app, middle-click toggles focus mode
-- (Mod+Alt+Z does too), right-click opens the dashboard.

local WELLBEING = "$HOME/.local/bin/wellbeing"
local DASHBOARD = "http://127.0.0.1:5600/pages/wellbeing/"

local total, topApp, focusMode, showTopApp = "--", "", false, false

local function render()
  barWidget.setGlyph(focusMode and "moon" or "hourglass")
  if showTopApp and topApp ~= "" then
    barWidget.setText(total .. " · " .. topApp)
  else
    barWidget.setText(total)
  end
  barWidget.setTooltip(table.concat({
    "Screen time today: " .. total,
    "Top app: " .. (topApp ~= "" and topApp or "none yet"),
    "Focus mode: " .. (focusMode and "on" or "off"),
    "Middle-click: focus mode · Right-click: dashboard",
  }, "\n"))
end

local function refresh()
  noctalia.runAsync(WELLBEING .. " --status", function(result)
    if result.exitCode ~= 0 then
      return
    end
    local ok, status = pcall(noctalia.json.decode, result.stdout)
    if not ok or type(status) ~= "table" then
      return
    end
    total = status.total or "--"
    topApp = status.top or ""
    focusMode = status.focus == "on"
    render()
  end)
end

-- As often as the helper refreshes its usage.
function update()
  noctalia.setUpdateInterval(60000)
  refresh()
end

function onClick()
  showTopApp = not showTopApp
  render()
end

-- The helper writes the new status as soon as it toggles; read it back.
function onMiddleClick()
  noctalia.runAsync(WELLBEING .. " --focus", function()
    refresh()
  end)
end

function onRightClick()
  noctalia.runAsync({ "xdg-open", DASHBOARD })
end

render()
