return {
  "Olical/conjure",
  config = function()
    -- vim.g["conjure#client#clojure#nrepl#eval#auto_require"] = false
    -- vim.g["conjure#filetype#lisp"] = "conjure.client.common-lisp.stdio"
  end,
  keys = function()
    return {
      { "<leader>cC", "<cmd>ConjureConnect<cr>", desc = "ConjureConnect" },
      { "<leader>ce", "<cmd>ConjureEval<cr>",    desc = "ConjureEval" },
    }
  end
}
