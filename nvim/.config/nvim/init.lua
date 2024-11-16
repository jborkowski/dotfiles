vim.g.mapleader = " "
vim.g.maplocalleader = ","

vim.o.relativenumber = false

local osc52 = {
  name = 'OSC 52',
  copy = {
    ['+'] = require('vim.ui.clipboard.osc52').copy('+'),
    ['*'] = require('vim.ui.clipboard.osc52').copy('*'),
  },
  paste = {
    ['+'] = require('vim.ui.clipboard.osc52').paste('+'),
    ['*'] = require('vim.ui.clipboard.osc52').paste('*'),
  },
}

vim.g.clipboard = osc52

vim.opt.clipboard = "unnamedplus"

vim.o.shell = 'zsh'

vim.o.hlsearch = true

vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2

vim.g.autowrite = true
vim.g.autoread = true
vim.g.incsearch = true
vim.g.nobackup = true
vim.g.noswapfile = true
vim.opt.swapfile = false

-- spell checker
vim.opt.spelllang = 'en_us'
vim.opt.spell = true

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup("plugins")

vim.g.vlime_enable_autodoc = true

-- exit from terminal mode
vim.api.nvim_set_keymap('t', '<Esc><Esc>', '<C-\\><C-n>', { noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>w', ':w<cr>', { noremap = true })

require('cursor_colemak')

-- quick access to newrt
vim.keymap.set('n', '<leader>e', ':Explore<CR>', { noremap = true, silent = true })
