return {
  'neovim/nvim-lspconfig',
  dependencies = {
    'williamboman/mason.nvim',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    'hrsh7th/nvim-cmp',
    'L3MON4D3/LuaSnip',
    'saadparwaiz1/cmp_luasnip',
  },
  config = function()
    local cmp_lsp = require 'cmp_nvim_lsp'
    vim.tbl_deep_extend('force', {}, vim.lsp.protocol.make_client_capabilities(), cmp_lsp.default_capabilities())
    require('lspconfig').lua_ls.setup {
      n_init = function(client)
        local path = client.workspace_folders[1].name
        if not vim.loop.fs_stat(path .. '/.luarc.json') and not vim.loop.fs_stat(path .. '/.luarc.jsonc') then
          client.config.settings = vim.tbl_deep_extend('force', client.config.settings, {
            Lua = {
              diagnostics = {
                globals = { 'vim', 'it', 'describe', 'before_each', 'after_each' },
              },
              runtime = {
                version = 'LuaJIT',
              },
              workspace = {
                checkThirdParty = false,
                library = {
                  vim.env.VIMRUNTIME,
                  -- "${3rd}/luv/library"
                  -- "${3rd}/busted/library",
                },
              },
            },
          })

          client.notify('workspace/didChangeConfiguration', { settings = client.config.settings })
        end
        return true
      end,
    }
    -- lspconfig.rust_analyzer.setup {
    --
    require'lspconfig'.tsserver.setup {}

    -- Global mappings.
    -- See `:help vim.diagnostic.*` for documentation on any of the below functions
    vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, {desc = "LSP: diagnostics"})
    vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
    vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
    vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

    -- Use LspAttach autocommand to only map the following keys
    -- after the language server attaches to the current buffer
    vim.api.nvim_create_autocmd('LspAttach', {
      group = vim.api.nvim_create_augroup('UserLspConfig', {}),
      callback = function(ev)
        -- Enable completion triggered by <c-x><c-o>
        vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

        -- Buffer local mappings.
        -- See `:help vim.lsp.*` for documentation on any of the below functions
        local opts = { buffer = ev.buf }
        vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, { buffer= ev.buf, desc = "LSP: Go to declaration" })
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer= ev.buf, desc = "LSP: Go to definition" })
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, { buffer= ev.buf, desc = "LSP: Go to implementation" })
        vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder,
          { buffer= ev.buf, desc = "LSP: Add workspace folder" }
        )
        vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder,
          { buffer= ev.buf, desc = "LSP: Remove workspace folder" }
        )
        vim.keymap.set('n', '<space>wl', function()
          print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, { buffer= ev.buf, desc = "LSP: List workspace folders" })

        vim.keymap.set('n', '<space>cD', vim.lsp.buf.type_definition,
          { buffer= ev.buf, desc = "LSP: Type definition" }
        )
        vim.keymap.set('n', '<space>cr', vim.lsp.buf.rename, { buffer= ev.buf, desc = "LSP: Rename" })
        vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action,
          { buffer= ev.buf, desc = "LSP: Code action" }
        )
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
        vim.keymap.set('n', '<space>cf', function()
          vim.lsp.buf.format { async = true }
        end, { buffer= ev.buf, desc = "LSP: Format document" })

        vim.keymap.set('n', '<space>cs', vim.lsp.buf.signature_help, 
          { buffer= ev.buf, desc = "LSP: Signatur Help" }
        )
      end,
    })
  end,
}
