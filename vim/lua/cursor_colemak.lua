-- cursor movement
--
--      ^
--      e
--  < n   o >
--      i
--      v

vim.keymap.set("n", "n", "h", { noremap = true, silent = true })
vim.keymap.set("n", "e", "j", { noremap = true, silent = true })
vim.keymap.set("n", "i", "k", { noremap = true, silent = true })
vim.keymap.set("n", "o", "l", { noremap = true, silent = true })

vim.keymap.set("v", "n", "h", { noremap = true, silent = true })
vim.keymap.set("v", "e", "j", { noremap = true, silent = true })
vim.keymap.set("v", "i", "k", { noremap = true, silent = true })
vim.keymap.set("v", "o", "l", { noremap = true, silent = true })

vim.keymap.set("n", "gi", "gk", { noremap = true, silent = true })
vim.keymap.set("n", "ge", "gj", { noremap = true, silent = true })

vim.keymap.set("n", "E", "5j", { noremap = true, silent = true })
vim.keymap.set("n", "I", "5k", { noremap = true, silent = true })
vim.keymap.set("n", "N", "5h", { noremap = true, silent = true })
vim.keymap.set("n", "O", "5l", { noremap = true, silent = true })


-- split the screens to up (horizontal), down (horizontal), left (vertical), right (vertical)
vim.keymap.set("n", "se", ":set nosplitbelow<CR>:split<CR>:set splitbelow<CR>")
vim.keymap.set("n", "si", ":set splitbelow<CR>:split<CR>")
vim.keymap.set("n", "so", ":set nosplitright<CR>:vsplit<CR>:set splitright<CR>")
vim.keymap.set("n", "sn", ":set splitright<CR>:vsplit<CR>")


-- Move to the end of this word
-- set h (same as n, cursor left) to 'end of word'
vim.keymap.set("n", "h", "e", { noremap = true, silent = true })

-- Insert key
vim.keymap.set("n", "k", "i", { noremap = true, silent = true })
vim.keymap.set("n", "K", "I", { noremap = true, silent = true })

-- do nothing
vim.keymap.set("n", "j", "", { noremap = true, silent = true })
vim.keymap.set("n", "J", "", { noremap = true, silent = true })


vim.keymap.set("n", "l", "o", { noremap = true, silent = true })


