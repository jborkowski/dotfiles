return {
  "Olical/conjure",
  ft = { "lisp", "scheme", "racket" },
  config = function()
    vim.g["conjure#extract#tree_sitter#enabled"] = true
vim.g["conjure#highlight#enabled"] = true
    -- vim.g["conjure#filetype#lisp"] = "conjure.client.common-lisp.stdio"
  end,
  -- keys = function()
  --   return {
  --     { "<leader>cC", "<cmd>ConjureConnect<cr>", desc = "ConjureConnect" },
  --     { "<leader>ce", "<cmd>ConjureEval<cr>",    desc = "ConjureEval" },
  --   }
  -- end
}
