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

require('cursor_colemak')
