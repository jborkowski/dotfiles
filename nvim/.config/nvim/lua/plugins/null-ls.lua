return {
  "nvimtools/none-ls.nvim",
  config = function()
    local null_ls = require("null-ls")
    local methods = require("null-ls.methods")
    local FORMATTING = methods.internal.FORMATTING

    local h = require("null-ls.helpers")
    local fourmolu_available = vim.fn.executable("fourmolu") == 1

    local sources = {}


    local fourmolu = h.make_builtin({
      name = "fourmolu",
      method = FORMATTING,
      filetypes = { "hs", "haskell" },
      generator_opts = {
        command = "fourmolu",
        args = {
          -- "--indentation=2",
          -- "--comma-style=leading",
          -- "--import-export-style=leading",
          -- "--indent-wheres=true",
          -- "--record-brace-space=true",
          -- "--respectful=true",
          -- "--haddock-style=multi-line",
          -- "--newlines-between-decls=1",
          "--stdin-input-file", "$FILENAME"
        },
        to_stdin = true,
      },
      factory = h.formatter_factory,
    })

    if fourmolu_available then
      table.insert(sources, fourmolu)
    else
      vim.notify("Fourmolu not found. Haskell formatting disabled.", vim.log.levels.WARN)
    end

    null_ls.setup({
      sources = sources,
      on_attach = function(client, bufnr)
        if client.supports_method("textDocument/formatting") then
          vim.api.nvim_create_autocmd("BufWritePre", {
            group = vim.api.nvim_create_augroup("Format", { clear = true }),
            buffer = bufnr,
            callback = function()
              vim.lsp.buf.format({ bufnr = bufnr })
            end,
          })
        end
      end,

    })
  end,
}
