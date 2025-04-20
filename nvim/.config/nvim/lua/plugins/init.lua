return {
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
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    keys = {
      { "<leader>ot", function() require("snacks.terminal").toggle() end,                                desc = "Toggle Terminal" },
      {
        '<leader>gB',
        function()
          require("snacks.git").blame_line()
        end,
        desc = 'Git Blame Line',
      },
      { "<leader>ot", function() require("snacks.terminal").toggle() end,                                desc = "Toggle Terminal" },
      { "<leader>gG", function() require("snacks").terminal({ "gitui" }) end,                            desc = "GitUi (cwd)" },
      { "<leader>gU", function() require("snacks").terminal({ "gitui" }, { cwd = vim.fn.getcwd() }) end, desc = "GitUi (root dir)" },
    },
    ---@type snacks.Config
    opts = {
      bigfile = { enabled = true },
      indent = { enabled = true },
      input = { enabled = false },
      git = {
        enable = true,
        win = {
          width = 0.6,
          height = 0.6,
          border = "rounded",
          title = " Git Blame ",
          title_pos = "center",
          footer = "",
          ft = "git",
        }
      },
      terminal = {
        enabled = true,
        win = {
          style = 'minimal',
          bo = {
            filetype = "snacks_terminal",
          },
          wo = {},
          keys = {
            q = 'hide',
            gf = function(self)
              local f = vim.fn.findfile(vim.fn.expand '<cfile>', '**')
              if f == '' then
                Snacks.notify.warn 'No file under cursor'
              else
                self:hide()
                vim.schedule(function()
                  vim.cmd('e ' .. f)
                end)
              end
            end,
            term_normal = {
              '<esc>',
              function(self)
                self.esc_timer = self.esc_timer or (vim.uv or vim.loop).new_timer()
                if self.esc_timer:is_active() then
                  self.esc_timer:stop()
                  vim.cmd 'stopinsert'
                else
                  self.esc_timer:start(250, 0, function() end)
                  return '<esc>'
                end
              end,
              mode = 't',
              expr = true,
              desc = 'Double escape to normal mode',
            },
          },

        }
      },
      quickfile = { enabled = true },
      statuscolumn = { enabled = true },
      words = { enabled = true },
    },
  },

  { "nvim-tree/nvim-web-devicons", lazy = true },

  { "stevearc/dressing.nvim",      event = "VeryLazy" },


}
