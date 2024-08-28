return {
  "restaumatic/nvim-dsl-lsp",
  event = "VeryLazy",
  enabled = true,
  config = function()
    require("dsl-lsp").setup({})
  end,
}
