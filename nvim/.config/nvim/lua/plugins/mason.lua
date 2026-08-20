return {
  {
    "williamboman/mason.nvim",
      cmd = { "Mason", "MasonInstall", "MasonUninstall", "MasonUpdate", "MasonLog" },
      opts = function(_, opts)
        opts.ui = {
          border = "rounded",
          height = 0.75,
          width = 0.75,
        }
      end,
  },
  { "williamboman/mason-lspconfig.nvim", lazy = true },
}
