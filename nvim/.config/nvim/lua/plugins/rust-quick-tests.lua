local keys = {
  {
    'K',
    function()
      require('rust-quick-tests').hover_actions()
    end,
    desc = 'Rust tests Hover actions',
  },
  {
    '<leader>l',
    function()
      require('rust-quick-tests').replay_last()
    end,
    desc = 'Replay last test',
  },
  {
    '<leader>gl',
    function()
      require('rust-quick-tests').snap_last()
    end,
    desc = 'Snap back to last test',
  },
}

return {
  'pgherveou/rust-quick-tests.nvim',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'mfussenegger/nvim-dap',
  },
  dev = true,
  lazy = true,
  config = true,
  ft = { 'rust' },
  cmd = { 'RustQuick' },
  keys = keys,
}
