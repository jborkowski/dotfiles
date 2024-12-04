return {
  lazy = true,
  enable = false,
  "Olical/conjure",
  ft = { "lisp", "scheme", "racket", "ros" },
  config = function()
    vim.g["conjure#extract#tree_sitter#enabled"] = true
    vim.g["conjure#highlight#enabled"] = true

    vim.g["conjure#client#lisp#swank#connection#auto_repl#hidden"] = true
    vim.g["conjure#client#lisp#swank#connection#auto_start"] = true
    vim.g["conjure#client#lisp#swank#command"] =
    "sbcl --load ~/quicklisp/setup.lisp --eval '(ql:quickload :swank)' --eval '(swank:create-server :port 4005 :dont-close t)'"
  end,
  keys = function()
    return {
      { "<leader>cC", "<cmd>ConjureConnect<cr>", desc = "ConjureConnect" },
      { "<leader>ce", "<cmd>ConjureEval<cr>",    desc = "ConjureEval" },
    }
  end
}
