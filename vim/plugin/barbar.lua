local map = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

-- Move to previous/next
map('n', '<LEADER>t,', '<Cmd>BufferPrevious<CR>', opts)
map('n', '<LEADER>t.', '<Cmd>BufferNext<CR>', opts)
-- Goto buffer in position...
map('n', '<LEADER>t1', '<Cmd>BufferGoto 1<CR>', opts)
map('n', '<LEADER>t2', '<Cmd>BufferGoto 2<CR>', opts)
map('n', '<LEADER>t3', '<Cmd>BufferGoto 3<CR>', opts)
map('n', '<LEADER>t4', '<Cmd>BufferGoto 4<CR>', opts)
map('n', '<LEADER>t5', '<Cmd>BufferGoto 5<CR>', opts)
map('n', '<LEADER>t6', '<Cmd>BufferGoto 6<CR>', opts)
map('n', '<LEADER>t7', '<Cmd>BufferGoto 7<CR>', opts)
map('n', '<LEADER>t8', '<Cmd>BufferGoto 8<CR>', opts)
map('n', '<LEADER>t9', '<Cmd>BufferGoto 9<CR>', opts)
map('n', '<LEADER>t0', '<Cmd>BufferLast<CR>', opts)
-- Pin/unpin buffer
map('n', '<LEADER>tp>', '<Cmd>BufferPin<CR>', opts)
-- Close buffer
map('n', '<LEADER>tc>', '<Cmd>BufferClose<CR>', opts)

vim.keymap.set("n", "<A-c>", "<Cmd>BufferClose<CR>", { silent = true })



