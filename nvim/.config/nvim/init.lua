vim.g.mapleader = " "

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
-- vim.g.autochdir = true
-- vim.o.autochdir = true

-- spell checker 
vim.opt.spelllang = 'en_us'
vim.opt.spell = true

if tonumber(os.date("%H")) < 6 then
  vim.o.background = "dark"
elseif tonumber(os.date("%H")) < 17 then
  vim.o.background = "light"
else
  vim.o.background = "dark"
end

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

-- exit from termial mode
vim.api.nvim_set_keymap('t', '<Esc><Esc>', '<C-\\><C-n>', { noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>w', ':w<cr>', { noremap = true })

require('cursor_colemak')
