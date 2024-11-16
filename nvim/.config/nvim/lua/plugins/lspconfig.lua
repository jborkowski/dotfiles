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
    'ThePrimeagen/refactoring.nvim',
  },
  config = function()
    local cmp_lsp = require 'cmp_nvim_lsp'
    local lspconfig = require 'lspconfig'
    local common = require('plugins.lspconfig.common')

    local on_attach = function(client, bufnr)
      common.set_mappings(client, bufnr)
    end

    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities.textDocument.completion.completionItem.snippetSupport = true

    local default_flags = { debounce_text_changes = 100 }
    local default_config = {
      capabilities = capabilities,
      on_attach = on_attach,
      flags = default_flags,
    }

    vim.tbl_deep_extend('force', {}, vim.lsp.protocol.make_client_capabilities(), cmp_lsp.default_capabilities())

    lspconfig.lua_ls.setup(vim.tbl_extend('force', default_config, {
      n_init = function(client)
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
                library = vim.list_extend(
                  { vim.env.VIMRUNTIME },
                  vim.fn.exists("/usr/local/openresty/lualib") == 1 and { "/usr/local/openresty/lualib" } or {}
                ),
              },
            },
          })

          client.notify('workspace/didChangeConfiguration', { settings = client.config.settings })
        end
        return true
      end
    }))

    lspconfig.hls.setup(vim.tbl_extend('force', default_config, {
      cmd = { "haskell-language-server-wrapper", "--lsp" },
      root_dir = lspconfig.util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"),
      settings = {
        haskell = {
          formattingProvider = "fourmolu",
          plugin = {
            hlint = {
              globalOn = true
            }
          }
        }
      }
    }))


    lspconfig.purescriptls.setup(vim.tbl_extend('force', default_config, {
      cmd = { "purescript-language-server", "--stdio" },
      filetypes = { "purescript" },
      root_dir = function(path)
        if path:match("/.spago/") then
          return nil
        end
        return lspconfig.util.root_pattern("bower.json", "psc-package.json", "spago.dhall")(path)
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

    lspconfig.rnix.setup(default_config)
    lspconfig.cssls.setup(default_config)
    lspconfig.html.setup(default_config)
    lspconfig.yamlls.setup(default_config)
    lspconfig.clangd.setup(default_config)
    lspconfig.marksman.setup(default_config)
    lspconfig.bashls.setup(default_config)
    lspconfig.ts_ls.setup(default_config)

    lspconfig.zls.setup(vim.tbl_extend('force', default_config, {
      settings = {
        zls = {
          -- zig_exe_path = '~/.local/bin/zls',
        }
      }
    }))

    local configs = require 'lspconfig.configs'

    if not configs.redsl then
      configs.redsl = {
        default_config = {
          cmd = { "dsl", "lsp" },
          filetypes = { "redsl", "haskell", "purescript", "typescript" },
          root_dir = function(fname)
            return lspconfig.util.find_git_ancestor(fname)
          end,
          settings = {},
        },
      }
    end

    lspconfig.redsl.setup(default_config)

    vim.api.nvim_create_autocmd('LspAttach', {
      group = vim.api.nvim_create_augroup('UserLspConfig', {}),
      callback = function(ev)
        -- Enable completion triggered by <c-x><c-o>
        vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
      end,
    })

    require('mason').setup()
    require('mason-lspconfig').setup({ automatic_installation = false })
  end,
}
