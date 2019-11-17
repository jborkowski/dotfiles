" Install vim-plug if we don't already have it
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

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

" Make your vim/neovim as smart as VSCode.
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

augroup interoMaps
  au!
  " Maps for intero. Restrict to Haskell buffers so the bindings don't collide.

  " Background process and window management
  au FileType haskell nnoremap <silent> <leader>is :InteroStart<CR>
  au FileType haskell nnoremap <silent> <leader>ik :InteroKill<CR>

  " Open intero/GHCi split horizontally
  au FileType haskell nnoremap <silent> <leader>io :InteroOpen<CR>
  " Open intero/GHCi split vertically
  au FileType haskell nnoremap <silent> <leader>iov :InteroOpen<CR><C-W>H
  au FileType haskell nnoremap <silent> <leader>ih :InteroHide<CR>

  " Reloading (pick one)
  " Automatically reload on save
  au BufWritePost *.hs InteroReload
  " Manually save and reload
  au FileType haskell nnoremap <silent> <leader>wr :w \| :InteroReload<CR>

  " Load individual modules
  au FileType haskell nnoremap <silent> <leader>il :InteroLoadCurrentModule<CR>
  au FileType haskell nnoremap <silent> <leader>if :InteroLoadCurrentFile<CR>

  " Type-related information
  " Heads up! These next two differ from the rest.
  au FileType haskell map <silent> <leader>t <Plug>InteroGenericType
  au FileType haskell map <silent> <leader>T <Plug>InteroType
  au FileType haskell nnoremap <silent> <leader>it :InteroTypeInsert<CR>

  " Navigation
  au FileType haskell nnoremap <silent> <leader>jd :InteroGoToDef<CR>

  " Managing targets
  " Prompts you to enter targets (no silent):
  au FileType haskell nnoremap <leader>ist :InteroSetTargets<SPACE>
augroup END

" Intero starts automatically. Set this if you'd like to prevent that.
let g:intero_start_immediately = 0

" Enable type information on hover (when holding cursor at point for ~1 second).
let g:intero_type_on_hover = 1

" Change the intero window size; default is 10.
let g:intero_window_size = 15

" Sets the intero window to split vertically; default is horizontal
let g:intero_vertical_split = 1

" OPTIONAL: Make the update time shorter, so the type info will trigger faster.
set updatetime=1000

