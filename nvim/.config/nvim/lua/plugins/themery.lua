return {
  "zaldih/themery.nvim",
  lazy = false,
  config = function()
    require("themery").setup({
      themes = { "github_light_colorblind", "github_dark_colorblind" },
      livePreview = true,
    })
  end,

  init = function()
    vim.keymap.set("n", "<leader>tt", function()
      local themery = require("themery")
      local currentTheme = themery.getCurrentTheme()
      if currentTheme and currentTheme.name == "github_light_colorblind" then
        themery.setThemeByName("github_dark_colorblind", true)
      else
        themery.setThemeByName("github_light_colorblind", true)
      end
    end, { noremap = true, desc = "Toggle theme" })
  end
}
