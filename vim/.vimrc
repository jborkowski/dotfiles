" Install vim-plug if we don't already have it
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'junegunn/vim-easy-align'
Plug 'https://github.com/junegunn/vim-github-dashboard.git'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }
Plug 'fatih/vim-go', { 'tag': '*' }
Plug 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

" Bash support 
Plug 'vim-scripts/bash-support.vim'
" Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }

" Projectionist
Plug 'tpope/vim-projectionist'

" Plugin 'mlent/ale' -- Has a small change for multi-line ghc errors, see below
Plug 'w0rp/ale'
Plug 'vim-airline/vim-airline'
Plug 'eagletmt/ghcmod-vim'
Plug 'Shougo/vimproc'

" Speed Dating Plugin
Plug 'tpope/vim-speeddating'

" Emacs Org mode support
Plug 'jceb/vim-orgmode'

" Rust Lang
Plug 'rust-lang/rust.vim'

" Dockerfile support
Plug 'ekalinin/dockerfile.vim'

" Lua
Plug 'xolox/vim-lua-ftplugin'
Plug 'xolox/vim-misc'

" Nginx
Plug 'chr4/nginx.vim'

" JSON & ProtoBuff
Plug 'elzr/vim-json' | Plug 'uarun/vim-protobuf'

" Make your vim/neovim as smart as VSCode.
Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'derekwyatt/vim-scala'

" Treeview
Plug 'scrooloose/nerdtree'

Plug 'purescript-contrib/purescript-vim'

"Plug 'parsonsmatt/intero-neovim'

" Initialize plugin system
call plug#end()

nnoremap <C-p> :<C-u>FZF<CR>

" tabstop:          Width of tab character
" softtabstop:      Fine tunes the amount of white space to be added
" shiftwidth        Determines the amount of whitespace to add in normal mode
" expandtab:        When on uses space instead of tabs
set tabstop     =2
set softtabstop =2
set shiftwidth  =2
set expandtab
set number
map <C-n> :NERDTreeToggle<CR>
