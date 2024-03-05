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
    "nvim-neorg/neorg",
    -- lazy-load on filetype
    ft = "norg",
    build = ":Neorg sync-parsers",
    dependencies = { 
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter"
    },
    keys = {
      {"<leader>nj", "<cmd>Neorg journal today<cr>", "Neorg today journal"}  
    },
    opts = {
      load = {
        ["core.defaults"] = {},
        ["core.completion"] = { config = { engine = "nvim-cmp", name = "[Norg]" } },
        ["core.concealer"] = {}, 
        ["core.integrations.nvim-cmp"] = {},
        ["core.dirman"] = { 
          config = {
            workspaces = {
              work = "~/org/work",
              home = "~/org",
            },
            default_workspace = "home",
          },
        },
--        ["core.ui.calendar"] = {},
        ["core.journal"] = {
          config = {
           strategy = "nested",
             workspace = "home",
           },
        },
        ["core.keybinds"] = {
        -- https://github.com/nvim-neorg/neorg/blob/main/lua/neorg/modules/core/keybinds/keybinds.lua
          config = {
            default_keybinds = true,
            neorg_leader = "<Leader>",
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
    'mrcjkb/rustaceanvim',
    version = '^4', 
    ft = { 'rust' },
  },
  {
    'nvim-orgmode/orgmode',
    config = function() require('orgmode').setup{} end,
    'akinsho/org-bullets.nvim',
  },
}
