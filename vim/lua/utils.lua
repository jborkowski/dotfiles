
local _M = {}

function _M.set_keymap(key, cmd)
  vim.keymap.set("n", key, cmd, { noremap = true,  silent = true })
end

return _M
