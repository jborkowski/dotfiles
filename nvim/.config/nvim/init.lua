vim.g.mapleader = " "

vim.o.relativenumber = true
vim.o.clipboard = "unnamedplus"

vim.o.shell = 'zsh'

vim.o.hlsearch = true 

vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2

vim.o.autowrite = true
vim.o.autoread = true 
vim.o.incsearch = true 
vim.o.nobackup = true 
vim.o.noswapfile = true 

require('plugins')
require('cursor_colemak')

