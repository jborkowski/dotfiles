local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

local function setup_fn(use)
  -- packer
  use 'wbthomason/packer.nvim'

  -- colorscheme 
  use 'EdenEast/nightfox.nvim'

  use 'nvim-tree/nvim-web-devicons'

  -- statusline
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'nvim-tree/nvim-web-devicons', opt = true }
  }
  
  -- tabline
  use {
    'romgrk/barbar.nvim',
    requires = { 'lewis6991/gitsigns.nvim', 'nvim-tree/nvim-web-devicons' }
  }

  -- treesitter
  use {
    'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'
  }

  use {
    'phaazon/hop.nvim',
    branch = 'v2', -- optional but strongly recommended
    config = function()
      -- you can configure Hop the way you like here; see :h hop-config
      require'hop'.setup { keys = 'etovxqpdygfblzhckisuran' }
    end
  }

  use {
    'neoclide/coc.nvim', 
    branch = 'release'
  }

  use 'dense-analysis/ale'
  use 'bkad/CamelCaseMotion'

  use {
    'preservim/nerdcommenter'
  }

  use { 
    'lukas-reineke/indent-blankline.nvim'
  }

  use {
    'nvim-telescope/telescope.nvim',
    'nvim-lua/plenary.nvim'
  }

  use 'tpope/vim-fugitive'
  use 'tpope/vim-sensible'
  use 'tpope/vim-surround'
  use 'SirVer/ultisnips'

  use 'JamshedVesuna/vim-markdown-preview'

  -- auto sync
  if packer_bootstrap then
     require('packer').sync()
  end
end

return require('packer').startup(setup_fn)