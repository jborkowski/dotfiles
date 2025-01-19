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

  config = true

}
