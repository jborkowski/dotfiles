return {
  "afonsofrancof/OSC11.nvim",
  lazy = false,
  priority = 1000,
  dependencies = {
    {
      "EdenEast/nightfox.nvim",
      opts = {
        options = {
          colorblind = {
            enable = true,
            simulate_only = false,
            severity = {
              protan = 0.4,
              deutan = 0.25,
              tritan = 0.0,
            },
          },
        },
      },
    },
    {
      "zaldih/themery.nvim",
      config = function()
        require("themery").setup({
          themes = { "github_light_colorblind", "github_dark_colorblind", "nightfox", "dayfox", "duskfox" },
          livePreview = true,
        })
      end,
    },
  },
  config = function()
    local themery = require("themery")

    -- Set initial theme
    themery.setThemeByName("duskfox", false)

    require("osc11").setup({
      on_dark = function()
        themery.setThemeByName("duskfox", false)
      end,
      on_light = function()
        themery.setThemeByName("dayfox", false)
      end,
    })
  end,
  init = function()
    vim.keymap.set("n", "<leader>tt", function()
      local themery = require("themery")
      local day_theme = "dayfox"
      local night_theme = "duskfox"
      local currentTheme = themery.getCurrentTheme()
      if currentTheme and currentTheme.name == night_theme then
        themery.setThemeByName(day_theme, true)
      else
        themery.setThemeByName(night_theme, true)
      end
    end, { noremap = true, desc = "Toggle theme" })
  end,
}
