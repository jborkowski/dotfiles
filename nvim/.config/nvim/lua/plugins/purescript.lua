return {
  {
    "srghma/nvimmer-ps",
    ft = "purescript",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
    config = function()
      require("nvimmer-ps").setup()
    end
  },
  {
    "purescript-contrib/purescript-vim",
    ft = "purescript"
  }
}
