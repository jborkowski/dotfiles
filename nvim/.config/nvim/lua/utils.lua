
local _M = {}

function _M.set_keymap(key, cmd)
  vim.keymap.set("n", key, cmd, { noremap = false,  silent = true })
end

_M.command = function(name, fn)
  vim.cmd(string.format('command! %s %s', name, fn))
end

_M.lua_command = function(name, fn)
  _M.command(name, 'lua ' .. fn)
end

return _M
