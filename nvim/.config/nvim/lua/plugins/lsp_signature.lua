return {
  "ray-x/lsp_signature.nvim",
  event = "InsertEnter",
  opts = {
    bind = false,
    handler_opts = {
      border = "rounded"
    }
  },
  config = function(_, opts)
    if vim.bo.filetype ~= "rust" then
      require("lsp_signature").on_attach(opts)
    end
  end,
}
