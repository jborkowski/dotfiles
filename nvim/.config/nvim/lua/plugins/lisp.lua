return {
  {
    "vlime/vlime",
    ft = { "lisp" },
    build = function()
      vim.fn.system("cd ~/.config/nvim/pack/vlime/start/vlime/lisp && make")
    end
  },
  {
    "gpanders/nvim-parinfer",
    ft = { "lisp", "scheme" },
  },
}
