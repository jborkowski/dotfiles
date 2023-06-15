vim.g.mapleader = " "

vim.o.relativenumber = false
vim.o.clipboard = "unnamedplus"

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

require('plugins')
require('cursor_colemak')

