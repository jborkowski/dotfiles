return {
  'akinsho/toggleterm.nvim',
  version = "*",
  config = true,
  cmd = 'ToggleTerm',
  keys = function(_, keys)
    local function toggleterm()
      local venv = vim.b['virtual_env']
      local term = require('toggleterm.terminal').Terminal:new({
        env = venv and { VIRTUAL_ENV = venv } or nil,
        count = vim.v.count > 0 and vim.v.count or 1,
      })
      term:toggle()
    end
    local mappings = {
      { '<leader>ot', mode = { 'n', 't' }, toggleterm, desc = 'Toggle terminal' },
    }
    return vim.list_extend(mappings, keys)
  end,

}
