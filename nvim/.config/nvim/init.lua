vim.g.mapleader = " "

vim.o.relativenumber = true
vim.o.clipboard = "unnamedplus"

vim.o.shell = 'zsh'

vim.g.hlsearch = true 

vim.g.autoread = true 
vim.g.autowrite = true 
vim.g.splitbelow = true

vim.g.nobackup = true 
vim.g.noswapfile = true 

vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2


require('plugins')
require('cursor_colemak')

