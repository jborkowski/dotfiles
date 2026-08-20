return {
  "EdenEast/nightfox.nvim",
  lazy = false,
  priority = 1000,
  opts = {
    options = {
      colorblind = {
        enable = true,
        simulate_only = false,
        severity = {
          protan = 0.4,
          deutan = 0.25,
          tritan = 0.0,
        },
      },
    },
  },
  config = function(_, opts)
    require("nightfox").setup(opts)

    local applying = false
    local function apply_system_theme()
      if applying then return end
      local theme = vim.o.background == "light" and "dayfox" or "duskfox"
      if vim.g.colors_name == theme then return end

      applying = true
      vim.cmd.colorscheme(theme)
      applying = false
    end

    -- Neovim 0.12 detects the terminal background at startup and after resume.
    -- Translate its 'background' value into the matching Nightfox variant.
    vim.api.nvim_create_autocmd("OptionSet", {
      pattern = "background",
      callback = apply_system_theme,
    })
    vim.api.nvim_create_autocmd("VimEnter", {
      once = true,
      callback = function()
        vim.schedule(apply_system_theme)
      end,
    })

    apply_system_theme()

    vim.keymap.set("n", "<leader>tt", function()
      vim.o.background = vim.o.background == "dark" and "light" or "dark"
      apply_system_theme()
    end, { desc = "Toggle light/dark theme" })
  end,
}
