return {
  "SmiteshP/nvim-navic",
  lazy = true,
  init = function()
    vim.g.navic_silence = true
  end,
  opts = function()
    return {
      separator = " ",
      highlight = true,
      depth_limit = 5,
    }
  end,
}
