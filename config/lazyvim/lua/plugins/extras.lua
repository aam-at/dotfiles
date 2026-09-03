-- Checks asynchronously (non-blocking) whether ActivityWatch is listening on
-- its default port, then invokes `callback(is_running)` on the main loop.
local function check_aw_running(callback)
  if not vim.system then
    -- Fallback for Neovim < 0.10, which lacks vim.system: skip the check
    -- rather than block startup on a synchronous io.popen call.
    callback(false)
    return
  end
  -- vim.system throws synchronously (not via the callback) if `nc` can't be
  -- spawned at all (e.g. not on PATH), so guard the spawn itself.
  local ok = pcall(vim.system, { "nc", "-z", "localhost", "5600" }, { text = true }, function(obj)
    vim.schedule(function()
      callback(obj.code == 0)
    end)
  end)
  if not ok then
    callback(false)
  end
end

return {
  -- tmux navigator
  {
    "christoomey/vim-tmux-navigator",
    cmd = {
      "TmuxNavigateLeft",
      "TmuxNavigateDown",
      "TmuxNavigateUp",
      "TmuxNavigateRight",
      "TmuxNavigatePrevious",
    },
    keys = {
      { "<c-h>", "<cmd><C-U>TmuxNavigateLeft<cr>" },
      { "<c-j>", "<cmd><C-U>TmuxNavigateDown<cr>" },
      { "<c-k>", "<cmd><C-U>TmuxNavigateUp<cr>" },
      { "<c-l>", "<cmd><C-U>TmuxNavigateRight<cr>" },
      { "<c-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>" },
    },
  },
  -- activity watch
  {
    "ActivityWatch/aw-watcher-vim",
    cmd = "AWStart",
    init = function()
      check_aw_running(function(is_running)
        vim.g.aw_enabled = is_running
        if is_running then
          vim.cmd("AWStart")
        end
      end)
    end,
  },
  -- Vim-visual-multi - Multiple cursors in vim
  {
    "mg979/vim-visual-multi",
    keys = {
      { "<C-n>", mode = { "n", "x" } },
      { "<C-Down>", mode = { "n", "x" } },
      { "<C-Up>", mode = { "n", "x" } },
    },
  },
  -- window picker
  {
    "s1n7ax/nvim-window-picker",
    name = "window-picker",
    event = "VeryLazy",
    version = "2.*",
    config = function()
      require("window-picker").setup()
    end,
  },
  -- direnv integration
  {
    "direnv/direnv.vim",
    event = "BufReadPost",
  },
  -- pencil colorscheme
  {
    "preservim/vim-colors-pencil",
    event = "VeryLazy",
  },
}
