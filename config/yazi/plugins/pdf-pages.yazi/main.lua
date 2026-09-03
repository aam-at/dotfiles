--- @since 25.5.31

local M = {}

local state = {
  pages = {},
  counts = {},
  loaded = false,
  active = nil,
}

local function state_file()
  local base = os.getenv("XDG_STATE_HOME") or (os.getenv("HOME") .. "/.local/state")
  return Url(base .. "/yazi/pdf-pages.tsv")
end

local function fail(job, err)
  if not err then
    return
  end

  return ya.preview_widget(job, ui.Text.parse(tostring(err)):area(job.area):wrap(ui.Wrap.YES))
end

local function shell_quote(value)
  value = tostring(value)
  return "'" .. value:gsub("'", [['"'"']]) .. "'"
end

local function load_state()
  if state.loaded then
    return
  end

  state.loaded = true

  local output, err = Command("sh"):arg({
    "-c",
    'test -f "$1" && cat "$1" || true',
    "sh",
    tostring(state_file()),
  }):output()

  if not output or not output.status.success then
    return
  end

  for line in output.stdout:gmatch("[^\r\n]+") do
    local key, page = line:match("^(.-)%s+([0-9]+)$")
    if key and page then
      state.pages[key] = tonumber(page) or 0
    end
  end
end

local function persist_state()
  local dir = state_file().parent
  if dir then
    fs.create("dir_all", dir)
  end

  local lines = {}
  for key, page in pairs(state.pages) do
    lines[#lines + 1] = string.format("%s\t%d", key, page)
  end
  table.sort(lines)

  local data = table.concat(lines, "\n")
  if #data > 0 then
    data = data .. "\n"
  end

  fs.write(state_file(), data)
end

local function persist_state_sync()
  local lines = {}
  for key, page in pairs(state.pages) do
    lines[#lines + 1] = string.format("%s\t%d", key, page)
  end
  table.sort(lines)

  local file = io and io.open(tostring(state_file()), "w")
  if not file then
    return
  end

  file:write(table.concat(lines, "\n"))
  if #lines > 0 then
    file:write("\n")
  end
  file:close()
end

local function set_skip(url, skip)
  load_state()
  state.pages[tostring(url)] = skip
  persist_state()
end

local function page_count(url)
  url = tostring(url)

  local cached = state.counts[url]
  if cached ~= nil then
    return cached > 0 and cached or nil
  end

  local output, err = Command("sh"):arg({
    "-lc",
    string.format("pdfinfo %s | awk '/^Pages:/ { print $2; exit }'", shell_quote(url)),
  }):output()

  if not output or not output.status.success then
    state.counts[url] = 0
    return nil
  end

  local count = tonumber(output.stdout:match("(%d+)")) or 0
  state.counts[url] = count
  return count > 0 and count or nil
end

function M:peek(job)
  load_state()

  local url = tostring(job.file.url)
  local wanted = state.pages[url] or 0
  local total = page_count(url)
  if total and wanted >= total then
    wanted = math.max(0, total - 1)
    state.pages[url] = wanted
    persist_state()
  end
  if state.active ~= url and job.skip == 0 and wanted > 0 then
    state.active = url
    ya.emit("peek", { wanted, only_if = job.file.url })
    return
  end

  local start, cache = os.clock(), ya.file_cache(job)
  if not cache then
    return
  end

  local ok, err, bound = self:preload(job)
  if bound and bound > 0 then
    return ya.emit("peek", { bound - 1, only_if = job.file.url, upper_bound = true })
  elseif not ok or err then
    return fail(job, err)
  end

  ya.sleep(math.max(0, rt.preview.image_delay / 1000 + start - os.clock()))

  local _, image_err = ya.image_show(cache, job.area)
  state.active = url
  set_skip(job.file.url, job.skip)

  if image_err then
    return fail(job, image_err)
  end
end

function M:seek(job)
  local h = cx.active.current.hovered
  if h and h.url == job.file.url then
    local url = tostring(job.file.url)
    local current = cx.active.preview and cx.active.preview.skip or state.pages[url] or 0

    local total = state.counts[url]
    local target
    if job.units < -1 then
      target = 0
    else
      local step = ya.clamp(-1, job.units, 1)
      target = math.max(0, current + step)
      if total then
        target = math.min(total - 1, target)
      end
    end
    if target == current then
      return
    end
    state.pages[url] = target
    persist_state_sync()
    ya.emit("peek", { target, only_if = job.file.url })
  end
end

function M:preload(job)
  local cache = ya.file_cache(job)
  if not cache or fs.cha(cache) then
    return true
  end

  local page = job.skip + 1
  local quality = tonumber(rt.preview.image_quality) or 90
  local input = shell_quote(job.file.url)
  local output_path = shell_quote(cache)
  local cmd = string.format(
    "pdftoppm -f %d -l %d -singlefile -jpeg -jpegopt quality=%d %s %s",
    page,
    page,
    quality,
    input,
    output_path
  )
  local output, err = Command("sh"):arg({ "-lc", cmd }):output()

  if not output then
    return true, Err("Failed to start `pdftoppm`, error: %s", err)
  end

  if not output.status.success then
    local pages = job.skip > 0 and tonumber(output.stderr:match("the last page %((%d+)%)"))
    local image = Url(cache .. ".jpg")
    if fs.cha(image) then
      ya.image_precache(image, cache)
      return true, nil, pages
    end
    return true, Err("Failed to convert PDF to image, stderr: %s", output.stderr), pages
  end

  local image = Url(cache .. ".jpg")
  ya.image_precache(image, cache)
  return true
end

return M
