return {
  {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release",
  },
  {
    "nvim-telescope/telescope-project.nvim",
    event = "BufWinEnter",
  },
}
