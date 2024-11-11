return {
  {
    "projekt0n/github-nvim-theme",
    lazy = false,
    priority = 1000,
    config = function()
      if vim.o.background == "light" then
        vim.cmd([[colorscheme github_light_colorblind]])
      else
        vim.cmd([[colorscheme github_dark_colorblind]])
      end
    end,

  },
  {
    "vhyrro/luarocks.nvim",
    priority = 1001,
    config = true,
    opts = {
      luarocks_build_args = {
        "--with-lua-include=/usr/include/",
      },
    },
  },

  {
    "dstein64/vim-startuptime",
    cmd = "StartupTime",
    init = function()
      vim.g.startuptime_tries = 10
    end,
  },

  { "nvim-tree/nvim-web-devicons", lazy = true },

  { "stevearc/dressing.nvim",      event = "VeryLazy" },

  {
    "Wansmer/treesj",
    keys = {
      { "J", "<cmd>TSJToggle<cr>", desc = "Join Toggle" },
    },
    opts = { use_default_keymaps = false, max_join_length = 150 },
  },
  {
    'nvim-orgmode/orgmode',
    config = function() require('orgmode').setup {} end,
    'akinsho/org-bullets.nvim',
  },
}
