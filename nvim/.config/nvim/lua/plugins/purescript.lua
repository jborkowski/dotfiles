return {
  {
    "srghma/nvimmer-ps",
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
