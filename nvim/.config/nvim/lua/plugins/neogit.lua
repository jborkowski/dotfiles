return {
  "NeogitOrg/neogit",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "sindrets/diffview.nvim",
    "nvim-telescope/telescope.nvim",
  },

  keys = function()
    return {
      { "<leader>gg", "<cmd>Neogit cwd=%:p:h <cr>",        desc = "Neogit" },
      { "<leader>gc", "<cmd>Neogit commit cwd=%:p:h <cr>", desc = "Neogit commit" },
    }
  end,

  config = function()
    vim.api.nvim_create_user_command('GitUI', function()
      vim.cmd("tabnew")
      vim.fn.termopen("gitui")
      vim.cmd("startinsert")
    end, {})
  end
}
