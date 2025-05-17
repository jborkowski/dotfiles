vim.g.rustaceanvim = function()
  return {
    tools = {
      test_executor_alias = "neotest",

      runnables = {
        use_telescope = true,
      },
    },
    server = {
      on_attach = function(client, bufnr)
        local common = require('plugins.lspconfig.common')
        common.set_mappings(client, bufnr, {
          ['K'] = {
            cmd = function()
              vim.cmd.RustLsp({ 'hover', 'actions' })
            end,
            desc = '[Rust] Lsp Hover',
          },
          ['<leader>l'] = { cmd = ':RustLsp! runnables<CR>', desc = '[Rust] Run last runnable' },
          ['<leader>ca'] = {
            cmd = function()
              vim.cmd.RustLsp('codeAction')
            end,
            desc = '[Rust] Code actions'
          },
          ['<leader>T'] = {
            cmd = function()
              require("neotest").run.run(vim.fn.expand("%"))
            end,
            desc = '[Rust] Run file tests'
          },
          ['<leader>D'] = { cmd = ':RustLsp! debug<CR>', desc = '[Rust] Debug target under cursor' },
          ['<leader>m'] = { cmd = ':RustLsp! expandMacro<CR>', desc = '[Rust] Expand macros' },
        })
      end,

      default_settings = {
        ['rust-analyzer'] = {
          checkOnSave = true,
          procMacro = {
            enable = true,
            ignored = {
              ['async-trait'] = { 'async_trait' },
              ['napi-derive'] = { 'napi' },
              ['async-recursion'] = { 'async_recursion' },
              ['async-std'] = { 'async_std' },
            },
          },
          files = {
            excludeUnlinkdFiles = true,
          },
          diagnostics = {
            enable = true,
            disabled = { "unlinked-file" },
          },
          completion = {
            snippets = {
              custom = {
                ['Collect into Vec'] = {
                  postfix = 'cv',
                  body = '${receiver}.collect::<Vec<_>>()',
                  description = 'Collects iterator into a Vec of an inferred type.',
                  scope = 'expr',
                },
                ['Collect'] = {
                  postfix = 'cl',
                  body = '${receiver}.collect::<$1>()',
                  description = 'Collects iterator into a Vec of an inferred type.',
                  scope = 'expr',
                },
                ['Filter Map Results'] = {
                  postfix = 'oks',
                  body = '${receiver}.filter_map(Result::ok)',
                  description =
                  'Filters an iterator of Results to Ok variants and maps it to its inner type.',
                  scope = 'expr',
                },
                ['Wrap with new'] = {
                  postfix = 'new',
                  body = '$1::new(${receiver})$0',
                  scope = 'expr',
                },
                ['Drop'] = {
                  postfix = 'drop',
                  body = 'drop(${receiver})',
                  scope = 'expr',
                },
                ['Clone Self'] = {
                  postfix = 'clsf',
                  body = 'let ${receiver} = ${receiver}.clone();$1',
                  scope = 'expr',
                },
              }
            }
          }
        }
      }
    }
  }
end


return {
  'mrcjkb/rustaceanvim',
  version = '*',
  ft = { 'rust' },
}
