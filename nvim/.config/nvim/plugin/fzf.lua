local map = vim.keymap.set; 


map("n", "<C-p>", "<Cmd>:FzfLua files<CR>", { silent = true })
map("n", "<C-f>", "<Cmd>:Rg<CR>", { silent = true })
map("n", "<C-h>", "<Cmd>:FzfLua oldfiles cwd=~<CR>", { silent = true })
map("n", "<C-q>", "<Cmd>:FzfLua builtin<CR>", { silent = true })
map("n", "<C-t>", "<Cmd>:FzfLua lines<CR>", { silent = true })
map("n", "<C-x>", "<Cmd>:FzfLua resume<CR>", { silent = true })
map("n", "<C-b>", "<Cmd>:FzfLua buffers<CR>", { silent = true })
-- noremap <silent> z= :FzfLua spell_suggest<CR

