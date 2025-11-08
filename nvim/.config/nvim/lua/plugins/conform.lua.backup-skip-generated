return {
  "stevearc/conform.nvim",
  event = { "BufWritePre" },
  cmd = { "ConformInfo" },
  config = function()
    local conform = require("conform")

    conform.setup({
      formatters_by_ft = {
        haskell = { "fourmolu" },
        hs = { "fourmolu" },
      },
      formatters = {
        fourmolu = {
          command = "fourmolu",
          args = {
            "--stdin-input-file",
            "$FILENAME",
          },
          stdin = true,
          cwd = require("conform.util").root_file({ "fourmolu.yaml", "cabal.project", "stack.yaml", ".git" }),
          condition = function()
            return vim.fn.executable("fourmolu") == 1
          end,
        },
      },
      format_on_save = {
        timeout_ms = 500,
        lsp_format = "fallback",
      },
    })
  end,
}
