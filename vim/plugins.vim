set runtimepath+=~/.config/nvim/

if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent call system('mkdir -p ~/.config/nvim/{autoload,bundle,cache,undo,backups,swaps}')
  silent call system('curl -fLo ~/.config/nvim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim')
  execute 'source  ~/.config/nvim/autoload/plug.vim'
  augroup plugsetup
    au!
    autocmd VimEnter * PlugInstall
  augroup end
endif

call plug#begin('~/.config/nvim/plugged')

" features
" Plug 'w0rp/ale'
" Plug 'tpope/vim-fugitive'
" Plug 'tpope/vim-sensible'
" Plug 'tpope/vim-surround'
" Plug 'tpope/vim-repeat'
" Plug 'tpope/vim-commentary'
" Plug 'SirVer/ultisnips'
" Plug 'JamshedVesuna/vim-markdown-preview'
" Plug 'juneedahamed/svnj.vim'
" Plug 'ctrlpvim/ctrlp.vim'

call plug#end()
