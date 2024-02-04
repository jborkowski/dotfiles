return {
  "nvimdev/dashboard-nvim",
  event = "VimEnter",
  enabled = true,

  keys = {
    {"<leader>aa", "<cmd>Dashboard<cr>", desc = "Dashboard display"}
  },

  opts = function()
    local opts = {
      theme = "doom",
      hide = {
        statusline = false,
      },

      config = {
        header = nil,
        -- stylua: ignore
        center = {
          {key = "f", icon = " ",  desc = " Find file",   action = "Telescope find_files" },
          {key = "n", icon = " ",  desc = " New file",    action = "ene | startinsert" },
          {key = "r", icon = " ",  desc = " Recent files",action = "Telescope oldfiles" },
          {key = "g", icon = " ",  desc = " Find text",   action = "Telescope live_grep" },
          {key = "l", icon = "",   desc = " Lazy",        action = "Lazy" },
          {key = "m", icon = " ",  desc = " Mason",       action = "Mason" },
          {key = "q", icon = " ",  desc = " Quit",        action = "qa" },
        },

        footer = function()
          local stats = require("lazy").stats()
          local ms = (math.floor(stats.startuptime * 100 + 0.5) / 100)
          return {"Neovim loaded " .. stats.loaded .. "/" .. stats.count .. " plugins in " .. ms .. "ms" }
        end,
      },
    }

    for _, button in ipairs(opts.config.center) do
      button.desc = button.desc .. string.rep(" ", 43 - #button.desc)
      button.key_format = "  %s"
    end

    -- close Lazy and re-open when the dashboard is ready
    if vim.o.filetype == "lazy" then
      vim.cmd.close()
      vim.api.nvim_create_autocmd("User", {
        pattern = "DashboardLoaded",
        callback = function()
          require("lazy").show()
        end,
      })
    end

    return opts
  end,
}
