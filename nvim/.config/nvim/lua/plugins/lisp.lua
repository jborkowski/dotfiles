return {
  {
    "monkoose/nvlime",
    dependencies = { "monkoose/parsley", "gpanders/nvim-parinfer" },
    ft = "lisp",
    lazy = true,
    cmp_enabled = true,
    enabled = false
  },
  {
    "gpanders/nvim-parinfer",
    ft = { "lisp", "scheme" },
  },
}
