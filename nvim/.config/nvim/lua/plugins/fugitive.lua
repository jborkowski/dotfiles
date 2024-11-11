return {
  "tpope/vim-fugitive",
  lazy = false,
  -- event = {"BufWinEnter"},
  enabled = true,

  cmd = "G",

  keys = {
    {"<leader>gP", "<cmd>G pull<cr>", desc = "Git pull" },
    {"<leader>gc", "<cmd>G commmit<cr>", desc = "Git commit" },
    {"<leader>gd", "<cmd>G diff<cr>", desc = "Git diff" },
    {"<leader>gl", "<cmd>G log<cr>", desc = "Git log" },
    {"<leader>gp", "<cmd>G push<cr>", desc = "Git push" },
    {"<leader>gs", "<cmd>G<cr>", desc = "Git status"},
    {"gh", "<cmd>diffget //2<cr>", desc = "Git diff - Chose left side" },
    {"gl", "<cmd>diffget //3<cr>", desc = "Git diff - Chose right size" }
  },
}
