return {
  "nvim-neorg/neorg",
  lazy = false,
  enabled = false,
  dependencies = {
    "luarocks.nvim",
  },
  keys = {
    { "<leader>nj", "<cmd>Neorg journal today<cr>",             "Neorg today journal" },
    { "<leader>ne", "<cmd>Neorg export to-file<cr>",            "Neorg export to file" },
    { "<leader>nf", "<cmd>Telescope neorg find_norg_files<cr>", "Neorg find file" },
  },
  config = function()
    require("neorg").setup({
      load = {
        ["core.defaults"] = {},
        ["core.completion"] = { engine = "nvim-cmp", config = { engine = "nvim-cmp", name = "[Norg]" } },
        ["core.concealer"] = {},
        ["core.syntax"] = {},
        ["core.integrations.nvim-cmp"] = {},
        ["core.integrations.telescope"] = {},
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
          config = {
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
        ["core.highlights"] = {},
        ["core.autocommands"] = {},
        ["core.summary"] = {},
      }
    })
  end,

}
