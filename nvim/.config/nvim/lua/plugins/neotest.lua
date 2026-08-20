return {
  "nvim-neotest/neotest",
  ft = "rust",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
  },
  opts = function(_, opts)
    opts.adapters = opts.adapters or {}
    vim.list_extend(opts.adapters, {
      require('rustaceanvim.neotest'),
    })
  end
}
