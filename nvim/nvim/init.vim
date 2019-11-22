" Install vim-plug if we don't already have it
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif


" enable mouse
set mouse=a

" theme
syntax enable
set termguicolors
set background=dark
let g:airline_theme='solarized_flood'
" map ; :Files

" Tab specific option
set number
set tabstop=2                   	"A tab is 2 spaces
set expandtab                   	"Always uses spaces instead of tabs
set softtabstop=2               	"Insert 2 spaces when tab is pressed
set shiftwidth=2                	"An indent is 2 spaces
set shiftround                  	"Round indent to nearest shiftwidth multipl
map <C-n> :NERDTreeToggle<CR>
nnoremap <C-p> :<C-u>FZF<CR>

filetype plugin on
filetype indent on
set hidden
set clipboard=unnamedplus
set wrap linebreak nolist

call plug#begin('~/.config/nvim/plugged')

Plug 'junegunn/vim-easy-align'
Plug 'https://github.com/junegunn/vim-github-dashboard.git'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
" Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }
Plug 'fatih/vim-go', { 'tag': '*' }
Plug 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }
" Bash support
Plug 'vim-scripts/bash-support.vim'
" Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }

Plug 'tpope/vim-projectionist'

" Plugin 'mlent/ale' -- Has a small change for multi-line ghc errors, see below
Plug 'w0rp/ale'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

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
Plug 'chr4/nginx.vim'
" JSON & ProtoBuff
Plug 'elzr/vim-json' | Plug 'uarun/vim-protobuf'

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'derekwyatt/vim-scala'

" Treeview
Plug 'scrooloose/nerdtree'
Plug 'purescript-contrib/purescript-vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'rust-lang/rust.vim'
Plug 'fatih/vim-go'
Plug 'parsonsmatt/intero-neovim'
Plug 'neomake/neomake'

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

Plug 'tpope/vim-sensible'		"Defaults
" Plug 'junegunn/seoul256.vim'		"Low-contrast theme
Plug 'scrooloose/nerdtree'
Plug 'frankier/neovim-colors-solarized-truecolor-only'
call plug#end()

