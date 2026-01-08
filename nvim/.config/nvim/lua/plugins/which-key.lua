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
    })
  end,
}
