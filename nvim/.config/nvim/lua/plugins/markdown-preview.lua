return {
  "iamcco/markdown-preview.nvim",
  enabled = false,
  lazy = false,
  keys = {
    {"<leader>mt", "<cmd>MarkdownPreviewToggle<cr>", desc = "Markdown Toggle Preview"},
    {"<leader>mp", "<cmd>MarkdownPreview<cr>", desc = "Markdown Preview"},
    {"<leader>ms", "<cmd>MarkdownPreviewStop<cr>", desc = "Markdown Stop Preview"},
  },
  ft = {"wiki", "markdown", "md"},
  build = function() vim.fn["mkdp#util#install"]() end,
}
