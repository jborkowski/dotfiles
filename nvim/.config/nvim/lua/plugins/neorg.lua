return {
  "nvim-neorg/neorg",
  -- lazy-load on filetype
  ft = "norg",
  build = ":Neorg sync-parsers",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter"
  },
  keys = {
    { "<leader>nj", "<cmd>Neorg journal today<cr>", "Neorg today journal" },
    { "<leader>ne", "<cmd>Neorg export to-file<cr>", "Neorg export to file" }
  },
  opts =  {
    load =   {
      ["core.defaults"] = {},
      ["core.completion"] = { engine = "nvim-cmp", config = { engine = "nvim-cmp", name = "[Norg]" } },
      ["core.concealer"] = {},
      ["core.syntax"] = {},
      ["core.integrations.nvim-cmp"] = {},
      ["core.dirman"] = {
        config = {
          workspaces = {
            work = "~/org/work",
            home = "~/org",
          },
          default_workspace = "home",
        },
      },
      ["core.journal"] = {
        config =   {
          strategy = "nested",
          workspace = "home",
        },
      },
      ["core.keybinds"] = {
        config = {
          default_keybinds = true,
          neorg_leader = "<Leader>",
        },
      },
      ["core.ui.calendar"] = {},
      ["core.ui"] = {},
      ["core.export"] = {},
      ["core.export.markdown"] = {},
    },
  },
}
