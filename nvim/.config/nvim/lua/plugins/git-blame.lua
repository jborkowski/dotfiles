return {
  "f-person/git-blame.nvim",
  event = "VeryLazy",
  config = function()
	  vim.cmd("highlight default link gitblame SpecialComment")
    vim.g.gitblame_enabled = false
  end,
  opts = {
    enabled = false,
    message_template = " <summary> • <date> • <author> • <<sha>>", 
    date_format = "%m-%d-%Y %H:%M:%S",
    virtual_text_column = 1,
  },
  keys = function()
    return {
      { "<leader>gb", "<cmd>GitBlameToggle<CR>", desc = "GitBlame Toggle" },
      { "<leader>gB", "<cmd>GitBlameCopySHA<CR>", desc = "GitBlame Copy SHA" },
    }
  end

}
