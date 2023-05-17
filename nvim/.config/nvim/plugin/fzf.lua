local function set_keymap_n(key, cmd)
  vim.keymap.set("n", key, cmd, { silent = true })
end


-- set_keymap_n("<C-P>", "<Cmd>:FzfLua files<CR>")
-- set_keymap_n("<C-f>", "<Cmd>:Rg<CR>")

