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
  {
    "folke/edgy.nvim",
    ---@module 'edgy'
    ---@param opts Edgy.Config
    opts = function(_, opts)
      for _, pos in ipairs({ "top", "bottom", "left", "right" }) do
        opts[pos] = opts[pos] or {}
        table.insert(opts[pos], {
          ft = "snacks_terminal",
          size = { height = 0.4 },
          title = "%{b:snacks_terminal.id}: %{b:term_title}",
          filter = function(_buf, win)
            return vim.w[win].snacks_win
                and vim.w[win].snacks_win.position == pos
                and vim.w[win].snacks_win.relative == "editor"
                and not vim.w[win].trouble_preview
          end,
        })
      end
    end,
  },

  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    keys = {
      { "<leader>ot", function() require("snacks.terminal").toggle() end, desc = "Toggle Terminal" },
    },
    ---@type snacks.Config
    opts = {
      bigfile = { enabled = true },
      indent = { enabled = true },
      input = { enabled = false },
      terminal = { enabled = true },
      quickfile = { enabled = true },
      statuscolumn = { enabled = true },
      words = { enabled = true },
    },
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
