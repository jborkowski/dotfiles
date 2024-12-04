return {
  lazy = true,
  "Olical/conjure",
  ft = { "lisp", "scheme", "racket", "ros" },
  config = function()
    vim.g["conjure#extract#tree_sitter#enabled"] = true
    vim.g["conjure#highlight#enabled"] = true
    vim.g["conjure#filetype#lisp"] = "conjure.client.common-lisp.stdio"
    vim.g["conjure#filetype#ros"] = "conjure.client.common-lisp.stdio"
    vim.g["conjure#client#common_lisp#stdio#command"] = "ros"
    vim.g["conjure#client#common_lisp#stdio#stdio_wrapper"] = "rlwrap"
  end,
  keys = function()
    return {
      { "<leader>cC", "<cmd>ConjureConnect<cr>", desc = "ConjureConnect" },
      { "<leader>ce", "<cmd>ConjureEval<cr>",    desc = "ConjureEval" },
    }
  end
}
