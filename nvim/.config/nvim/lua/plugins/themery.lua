return {
  "zaldih/themery.nvim",
  lazy = false,
  config = function()
    require("themery").setup({
      themes = { "github_light_colorblind", "github_dark_colorblind", "nightfox", "dayfox", "duskfox" },
      livePreview = true,
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
  end
}
