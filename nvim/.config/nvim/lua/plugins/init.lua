return {
  {
    "folke/tokyonight.nvim",
    style = "night",
    lazy = false,
    priority = 1000, 
    config = function()
      -- load the colorscheme here
      vim.cmd([[colorscheme tokyonight]])
    end,
  },

  {
    "nvim-neorg/neorg",
    -- lazy-load on filetype
    ft = "norg",
    build = ":Neorg sync-parsers",
    dependencies = { 
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter"
    },
    opts = {
      load = {
        ["core.defaults"] = {},
        ["core.concealer"] = {}, 
        ["core.dirman"] = { 
           config = {
            workspaces = {
              notes = "~/org",
            },
          },
        },
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

  { "stevearc/dressing.nvim", event = "VeryLazy" },
  
  {
    "Wansmer/treesj",
    keys = {
      { "J", "<cmd>TSJToggle<cr>", desc = "Join Toggle" },
    },
    opts = { use_default_keymaps = false, max_join_length = 150 },
  },
  {
    "neoclide/coc.nvim",
    branch = 'release',

    ft = { 'haskell', 'purs'  },
    keys = {
      { "gd", "<Plug>(coc-definition)<cr>", desc = "Coc: Go to Definition" },
      { "gy", "<Plug>(coc-type-definition)<cr>", desc = "Coc: Go to Type Definition" },
      { "gi", "<Plug>(coc-implementation)<cr>", desc = "Coc: Go to implementation" },
      { "gr", "<Plug>(coc-references)<cr>", desc = "Coc: Go To References" },
    }


  },
  {
    'mrcjkb/rustaceanvim',
    version = '^4', 
    ft = { 'rust' },
  },
  {
    'nvim-orgmode/orgmode',
    config = function() require('orgmode').setup{} end,
    'akinsho/org-bullets.nvim',
  }
}
