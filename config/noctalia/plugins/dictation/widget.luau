-- Dictation state from scripts/toggle-dictation.sh, the Linux counterpart of
-- windots' YASB dictation icon: a microphone when idle, a red dot while
-- recording, an hourglass while transcribing. Click toggles dictation.

local STATE = {
  idle = { glyph = "microphone", tip = "Dictation: idle" },
  recording = { glyph = "player-record", tip = "Dictation: recording" },
  transcribing = { glyph = "hourglass", tip = "Dictation: transcribing" },
}

local CHECK = [[
d="${XDG_RUNTIME_DIR:-/tmp}/dictation"
if [ -f "$d/pid" ] && [ "$(cat /proc/$(cat "$d/pid")/comm 2>/dev/null)" = pw-record ]; then echo recording
elif [ -f "$d/transcribe.pid" ] && kill -0 "$(cat "$d/transcribe.pid")" 2>/dev/null; then echo transcribing
else echo idle; fi
]]

local function show(name)
  local s = STATE[name] or STATE.idle
  barWidget.setGlyph(s.glyph)
  barWidget.setTooltip(s.tip .. "\nClick: toggle dictation (Mod+S)")
end

function update()
  noctalia.setUpdateInterval(1000)
  noctalia.runAsync({ "sh", "-c", CHECK }, function(result)
    show((result.stdout or ""):gsub("%s+", ""))
  end)
end

function onClick()
  noctalia.runAsync({ "sh", "-c", "toggle-dictation.sh" })
end

show("idle")
