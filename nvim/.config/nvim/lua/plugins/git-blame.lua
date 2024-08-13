return {
  "f-person/git-blame.nvim",
  event = "VeryLazy",
  config = function()

  end,
  opts = {
    enabled = false,
    message_template = " <summary> • <date> • <author> • <<sha>>", 
    date_format = "%m-%d-%Y %H:%M:%S",
    virtual_text_column = 1,
  },
  keys = function()
    return {
      { "<leader>gbt", "<cmd>GitBlameToggle<CR>", desc = "GitBlame Toggle" },
      { "<leader>gbc", "<cmd>GitBlameCopySHA<CR>", desc = "GitBlame Copy SHA" },
    }
  end

}
