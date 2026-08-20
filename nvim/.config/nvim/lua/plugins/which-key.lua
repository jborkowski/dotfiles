return {
  "folke/which-key.nvim",
  enabled = true,
  event = "VeryLazy",

  -- Restrict the popup to configured leader namespaces. The previous broad
  -- triggers for g/c/v/registers/windows exposed a large amount of builtin noise.
  opts = {
    triggers = {
      { "<leader>", mode = { "n", "v" } },
      { "<localleader>", mode = { "n", "v" } },
    },
  },

  config = function(_, opts)
    local wk = require("which-key")
    wk.setup(opts)

    wk.add({
      { "<localleader>", group = "Local leader (,)" },
      { "<leader>b", group = "Buffers" },
      { "<leader>c", group = "Code / LSP" },
      { "<leader>f", group = "Files" },
      { "<leader>g", group = "Git" },
      { "<leader>gL", group = "GitHub lists" },
      { "<leader>h", group = "Harpoon" },
      { "<leader>o", group = "Open" },
      { "<leader>p", group = "Project" },
      { "<leader>s", group = "Sessions" },
      { "<leader>t", group = "Toggle" },
      { "<leader>x", group = "Diagnostics" },
    })
  end,
}
