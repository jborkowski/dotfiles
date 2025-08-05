return {
  "dmtrKovalenko/fff.nvim",
  build = "cargo build --release",
  opts = {
  },
  keys = {
    {
      "ff",
      function()
        require("fff").find_files()
      end,
      desc = "Open file picker",
    },
    {
      "<leader>ff",
      function()
        require("fff").find_files()
      end,
      desc = "Open file picker",
    },
    {
      "pf",
      function()
        require("fff").find_in_git_root()
      end,
      desc = "Find project file",
    },

  },
}
