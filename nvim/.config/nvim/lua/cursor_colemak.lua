local utils = require('utils')

-- cursor movement
--
--      ^
--      e
--  < n   o >
--      i
--      v

utils.set_keymap("n", "h")
utils.set_keymap("e", "j")
utils.set_keymap("i", "k")
utils.set_keymap("o", "l")

vim.keymap.set("v", "n", "h")
vim.keymap.set("v", "e", "j")
vim.keymap.set("v", "i", "k")
vim.keymap.set("v", "o", "l")

utils.set_keymap("gi", "gk")
utils.set_keymap("ge", "gj")

utils.set_keymap("E", "5j")
utils.set_keymap("I", "5k")
utils.set_keymap("N", "5h")
utils.set_keymap("O", "5l")

-- Disable the default s key
utils.set_keymap("s", "<nop>")

-- moving the cursor around windows
vim.keymap.set("n", "sw", "<C-w>w", { noremap = true })
vim.keymap.set("n", "sn", "<C-w>h", { noremap = true })
vim.keymap.set("n", "se", "<C-w>j", { noremap = true })
vim.keymap.set("n", "si", "<C-w>k", { noremap = true })
vim.keymap.set("n", "so", "<C-w>l", { noremap = true })

-- split the screens to horizontal (down) and vertical (right)
vim.keymap.set("n", "sh", ":set splitbelow<CR>:split<CR>", { noremap = true })
vim.keymap.set("n", "sv", ":set nosplitright<CR>:vsplit<CR>:set splitright<CR>", { noremap = true })

-- Ctrl + e or i will move up/down the view port without moving the cursor
utils.set_keymap("<C-i>", "5<C-y>")
utils.set_keymap("<C-e>", "5<C-e>")

-- Move to the end of this word
-- set h (same as n, cursor left) to 'end of word'
utils.set_keymap("h", "e")

-- Insert key
utils.set_keymap("k", "i")
utils.set_keymap("K", "I")

-- do nothing
utils.set_keymap("j", "")
utils.set_keymap("J", "")


utils.set_keymap("l", "o")

local buffer_settings = vim.api.nvim_create_augroup('buffer_settings', {})

vim.api.nvim_create_autocmd('FileType', {
   desc = 'Use colemak keymap in buffer',
   group = buffer_settings,
   pattern = {
      'help',
      'lspinfo',
      'man',
      'netrw',
      'qf',
   },
   callback = function()
      vim.keymap.set('n', 'n', 'h', { buffer = 0 })
      vim.keymap.set('n', 'e', 'j', { buffer = 0 })
      vim.keymap.set('n', 'i', 'k', { buffer = 0 })
      vim.keymap.set('n', 'o', 'l', { buffer = 0 })

      vim.keymap.set('n', 'h', 'n', { buffer = 0 })
      vim.keymap.set('n', 'j', 'e', { buffer = 0 })
      vim.keymap.set('n', 'k', 'i', { buffer = 0 })
      vim.keymap.set('n', 'l', 'o', { buffer = 0 })
 
   end,
})

local map = vim.keymap.set 

-- map('n', 'w', '<Plug>CamelCaseMotion_w', { silent = true })
-- map('n', 'b', '<Plug>CamelCaseMotion_b', { silent = true })
-- map('n', 'h', '<Plug>CamelCaseMotion_e', { silent = true })
-- map('n', 'gh', '<Plug>CamelCaseMotion_ge', { silent = true })

-- Term
vim.g.neoterm_autoscroll = true 
-- autocmd TermOpen term://* startinsert
map('t', '<C-n>', '<C-\\><C-N>')
