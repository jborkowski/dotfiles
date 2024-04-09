local function is_enabled()
  local enabled_for = { ['Nundu.local'] = true, ['hasurian'] = true }

  -- return enabled_for[vim.fn.hostname()] or false
  return false
end

return {
  'Exafunction/codeium.vim',
  event = 'BufEnter',
  enabled = is_enabled(),
  init = function()
    vim.g.codeium_disable_bindings = 1
  end,
  keymaps = {
    { '<leader>tc', '<cmd>CodeiumToggle<cr>', desc = 'Toggle Codeium' },
  },
  config = function()
    vim.keymap.set('i', '<tab>', vim.fn['codeium#Accept'], { noremap = true, expr = true, desc = 'Codeium Accept' })
    vim.keymap.set('i', '<m-]>', function() return vim.fn['codeium#CycleCompletions'](1) end, { noremap = true, expr = true, desc = 'Next Codeium Completion' })
    vim.keymap.set('i', '<m-[>', function() return vim.fn['codeium#CycleCompletions'](-1) end, { noremap = true, expr = true, desc = 'Prev Codeium Completion' })
    vim.keymap.set('i', '<m-x>', vim.fn['codeium#Clear'], { noremap = true, expr = true, desc = 'Codeium Clear' })
    vim.keymap.set('i', '<m-i>', vim.fn['codeium#Complete'], { noremap = true, expr = true, desc = 'Codeium Complete' })
  end
}
