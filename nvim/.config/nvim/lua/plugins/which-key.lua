return {
  "folke/which-key.nvim",
  enabled = true,
  keys = { "<leader>", "<c-r>", "<c-w>", '"', "`", "c", "v", "g" },
  event = { "InsertEnter" },

  opts = {
    plugins = { spelling = true }
  },

  config = function(_, opts)
    local wk = require("which-key")
    wk.setup(opts)

    wk.add({
      { "<leader>a",  group = "Avante" },
      { "<leader>c",  group = "Actions" },
      { "<leader>g",  group = "Git" },
      { "<leader>h",  group = "Harpoon" },
      { "<leader>t",  group = "Toggle..." },
      { "<leader>h",  group = "Harpoon" },
      { "<leader>p",  group = "Project" },
      { "<leader>f",  group = "Files" },
      { "<leader>n",  group = "Notes" },
      { "<leader>nd", "<cmd>ObsidianToday<cr>",  desc = "Daily Note" },
      { "<leader>ns", "<cmd>ObsidianSearch<cr>", desc = "Search" },
      { "<leader>nn", "<cmd>ObsidianNew<cr>",    desc = "New Note" },
      { "<leader>no", "<cmd>ObsidianOpen<cr>",   desc = "Open Note" },
    })
  end,
}
