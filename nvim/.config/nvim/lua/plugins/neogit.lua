return {
  "NeogitOrg/neogit",
  dependencies = {
    "nvim-lua/plenary.nvim",
    -- "sindrets/diffview.nvim",
    "nvim-telescope/telescope.nvim",
  },

  keys = function()
    return {
      { "<leader>gg", "<cmd>Neogit cwd=%:p:h <cr>",        desc = "Neogit" },
      { "<leader>gu", "<cmd>Gitu <cr>",                    desc = "Gitu (rs)" },
      { "<leader>gc", "<cmd>Neogit commit cwd=%:p:h <cr>", desc = "Neogit commit" },
    }
  end,

  config = {
    disable_hint = false,
    prompt_force_push = false,
    filewatcher = {
      interval = 1000,
      enabled = false,
    },
    graph_style = "kitty",
    process_spinner = true,
    auto_show_console = false,
  }

}
