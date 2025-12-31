return {
  'neovim/nvim-lspconfig',
  lazy = false,
  dependencies = {
    'williamboman/mason.nvim',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    'hrsh7th/nvim-cmp',
    'L3MON4D3/LuaSnip',
    'saadparwaiz1/cmp_luasnip',
    'srghma/nvimmer-ps',
    'ThePrimeagen/refactoring.nvim',
  },
  config = function()
    local cmp_lsp = require 'cmp_nvim_lsp'
    local common = require('plugins.lspconfig.common')
    local navic = require("nvim-navic")

    local function safe_enable(server, cmd)
      if cmd and vim.fn.executable(cmd) == 0 then
        return
      end
      pcall(vim.lsp.enable, server)
    end

    local on_attach = function(client, bufnr)
      common.set_mappings(client, bufnr)
      navic.attach(client, bufnr)
    end

    local capabilities = vim.tbl_deep_extend(
      'force',
      vim.lsp.protocol.make_client_capabilities(),
      cmp_lsp.default_capabilities()
    )
    capabilities.textDocument.completion.completionItem.snippetSupport = true

    local default_flags = { debounce_text_changes = 100 }
    local default_config = {
      capabilities = capabilities,
      on_attach = on_attach,
      flags = default_flags,
    }

    vim.lsp.config('lua_ls', vim.tbl_extend('force', default_config, {
      on_init = function(client)
        local path = client.workspace_folders[1].name
        if not vim.loop.fs_stat(path .. '/.luarc.json') and not vim.loop.fs_stat(path .. '/.luarc.jsonc') then
          client.config.settings = vim.tbl_deep_extend('force', client.config.settings, {
            Lua = {
              diagnostics = {
                telemetry = { enable = false },
                globals = { 'vim', 'hs' },
              },
              runtime = {
                version = 'LuaJIT',
              },
              workspace = {
                checkThirdParty = false,
                library = {
                  vim.env.VIMRUNTIME
                }
              },
            },
          })

          client.notify('workspace/didChangeConfiguration', { settings = client.config.settings })
        end
        return true
      end
    }))
    safe_enable('lua_ls', 'lua-language-server')

    vim.lsp.config('hls', vim.tbl_extend('force', default_config, {
      cmd = { "haskell-language-server-wrapper", "--lsp" },
      root_dir = function(bufnr)
        return vim.fs.root(bufnr, {"*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"})
      end,
      settings = {
        haskell = {
          formattingProvider = "fourmolu",
          plugin = {
            hlint = {
              globalOn = true
            },
            fourmolu = {
              globalOn = false
            }
          }
        }
      }
    }))
    safe_enable('hls', 'haskell-language-server-wrapper')

    vim.lsp.config('purescriptls', vim.tbl_extend('force', default_config, {
      on_attach = function(client, bufnr)
        require("nvimmer-ps").setup_on_attach(client, bufnr)
      end,
      on_init = function(client)
        require("nvimmer-ps").setup_on_init(client)
      end,
      filetypes = { "purescript" },
      root_dir = function(bufnr)
        local path = vim.api.nvim_buf_get_name(bufnr)
        if path:match("/.spago/") then
          return nil
        end
        return vim.fs.root(bufnr, {"bower.json", "psc-package.json", "spago.dhall"})
      end,
      settings = {
        purescript = {
          formatter = "purs-tidy",
          addPscPackageSources = true,
          addNpmPath = true
        }
      },
      flags = {
        debounce_text_changes = 150,
      }
    }))
    safe_enable('purescriptls', 'purescript-language-server')

    vim.lsp.config('rnix', default_config)
    safe_enable('rnix', 'rnix-lsp')

    vim.lsp.config('cssls', default_config)
    safe_enable('cssls', 'vscode-css-language-server')

    vim.lsp.config('html', default_config)
    safe_enable('html', 'vscode-html-language-server')

    vim.lsp.config('clangd', default_config)
    safe_enable('clangd', 'clangd')

    vim.lsp.config('marksman', default_config)
    safe_enable('marksman', 'marksman')

    vim.lsp.config('bashls', default_config)
    safe_enable('bashls', 'bash-language-server')

    vim.lsp.config('ts_ls', default_config)
    safe_enable('ts_ls', 'typescript-language-server')

    vim.lsp.config('svelte', default_config)
    safe_enable('svelte', 'svelteserver')

    vim.lsp.config('terraform_lsp', vim.tbl_extend('force', default_config, {
      filetypes = { "terraform", "terraform-vars", "hcl" },
      root_dir = function(bufnr)
        return vim.fs.root(bufnr, {".terraform", ".git"})
      end,
    }))
    safe_enable('terraform_lsp', 'terraform-lsp')

    vim.lsp.config('gopls', vim.tbl_extend('force', default_config, {
      cmd = { "gopls" },
      filetypes = { "go", "gomod", "gowork", "gotmpl" },
      root_dir = function(bufnr)
        return vim.fs.root(bufnr, {"go.work", "go.mod", ".git"})
      end,
      settings = {
        gopls = {
          analyses = {
            unusedparams = true,
          },
          staticcheck = true,
        },
      },
    }))
    safe_enable('gopls', 'gopls')

    vim.lsp.config('postgres_lsp', default_config)
    safe_enable('postgres_lsp', 'postgres_lsp')

    vim.lsp.config('pylsp', default_config)
    safe_enable('pylsp', 'pylsp')

    vim.lsp.config('yamlls', default_config)
    safe_enable('yamlls', 'yaml-language-server')

    vim.lsp.config('zls', vim.tbl_extend('force', default_config, {
      settings = {
        zls = {
        }
      }
    }))
    safe_enable('zls', 'zls')

    vim.lsp.config('redsl', vim.tbl_extend('force', default_config, {
      cmd = { "dsl", "lsp" },
      filetypes = { "redsl", "haskell", "purescript", "typescript" },
      root_dir = function(bufnr)
        return vim.fs.root(bufnr, {".git"})
      end,
    }))
    safe_enable('redsl', 'dsl')

    vim.lsp.config('circleci', vim.tbl_extend('force', default_config, {
      cmd = { "circleci-yaml-language-server", "--stdio" },
      filetypes = { "yaml", "yml" },
      root_dir = function(bufnr)
        return vim.fs.root(bufnr, {".circleci/config.yml", ".git"})
      end,
      single_file_support = true,
    }))
    safe_enable('circleci', 'circleci-yaml-language-server')

    vim.lsp.config('harper_ls', default_config)
    safe_enable('harper_ls', 'harper-ls')

    vim.lsp.config('solargraph', default_config)
    safe_enable('solargraph', 'solargraph')

    vim.api.nvim_create_autocmd('LspAttach', {
      group = vim.api.nvim_create_augroup('UserLspConfig', {}),
      callback = function(ev)
        -- Enable completion triggered by <c-x><c-o>
        vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
      end,
    })

    require('mason').setup()
    require('mason-lspconfig').setup({
      automatic_enable = {
        exclude = {
          "rust_analyzer",
          "hls",
          "purescript"
        }
      }
    })
  end,
}
