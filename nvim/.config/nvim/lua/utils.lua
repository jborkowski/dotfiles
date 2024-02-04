
local _M = {}

function _M.set_keymap(key, cmd)
  vim.keymap.set("n", key, cmd, { noremap = false,  silent = true })
end

return _M
