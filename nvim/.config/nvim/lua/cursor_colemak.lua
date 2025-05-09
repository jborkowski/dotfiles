local utils = require('utils')

utils.set_keymap("J", "5j")
utils.set_keymap("K", "5k")
utils.set_keymap("H", "5h")
utils.set_keymap("L", "5l")


-- moving the cursor around windows
vim.keymap.set("n", "sw", "<C-w>w", { noremap = true })
vim.keymap.set("n", "sn", "<C-w>h", { noremap = true })
vim.keymap.set("n", "se", "<C-w>j", { noremap = true })
vim.keymap.set("n", "si", "<C-w>k", { noremap = true })
vim.keymap.set("n", "so", "<C-w>l", { noremap = true })

-- split the screens to horizontal (down) and vertical (right)
vim.keymap.set("n", "sh", ":set splitbelow<CR>:split<CR>", { noremap = true })
vim.keymap.set("n", "sv", ":set nosplitright<CR>:vsplit<CR>:set splitright<CR>", { noremap = true })

local map = vim.keymap.set

map('n', 'w', '<Plug>CamelCaseMotion_w', { silent = true })
map('n', 'b', '<Plug>CamelCaseMotion_b', { silent = true })
map('n', 'e', '<Plug>CamelCaseMotion_e', { silent = true })
map('n', 'ge', '<Plug>CamelCaseMotion_ge', { silent = true })

-- Tab navigation
map('n', 'tn', ':tabnew<CR>', { noremap = true, silent = true, desc = "Tab New" })
map('n', 't[', ':tabnext<CR>', { noremap = true, silent = true, desc = "Next tab" })
map('n', 't]', ':tabprevious<CR>', { noremap = true, silent = true, desc = "Previous tab" })
