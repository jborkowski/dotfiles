local utils = require('utils')

utils.set_keymap("J", "5j")
utils.set_keymap("gk", "5k")  -- Use gk instead of K
utils.set_keymap("H", "5h")
utils.set_keymap("L", "5l")


-- moving the cursor around windows (skip in git buffers)
local function not_git_buffer()
  local ft = vim.bo.filetype
  return ft ~= "NeogitStatus" and ft ~= "NeogitCommitMessage" and ft ~= "NeogitPopup"
end

vim.keymap.set("n", "sw", function()
  if not_git_buffer() then vim.cmd("wincmd w") end
end, { noremap = true })
vim.keymap.set("n", "sn", function()
  if not_git_buffer() then vim.cmd("wincmd h") end
end, { noremap = true })
vim.keymap.set("n", "se", function()
  if not_git_buffer() then vim.cmd("wincmd j") end
end, { noremap = true })
vim.keymap.set("n", "si", function()
  if not_git_buffer() then vim.cmd("wincmd k") end
end, { noremap = true })
vim.keymap.set("n", "so", function()
  if not_git_buffer() then vim.cmd("wincmd l") end
end, { noremap = true })

-- split the screens to horizontal (down) and vertical (right)
vim.keymap.set("n", "sh", function()
  if not_git_buffer() then vim.cmd("set splitbelow | split") end
end, { noremap = true })
vim.keymap.set("n", "sv", function()
  if not_git_buffer() then vim.cmd("set nosplitright | vsplit | set splitright") end
end, { noremap = true })

local map = vim.keymap.set

-- Tab navigation
map('n', 'tn', ':tabnew<CR>', { noremap = true, silent = true, desc = "Tab New" })
map('n', 't[', ':tabnext<CR>', { noremap = true, silent = true, desc = "Next tab" })
map('n', 't]', ':tabprevious<CR>', { noremap = true, silent = true, desc = "Previous tab" })
