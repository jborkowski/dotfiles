vim.g.rustaceanvim = function()
  return {
    tools = {
      runnables = {
        use_telescope = true,
      },
      hover_actions = {
        replace_builtin_hover = true,
      },
    },
    server = {

      on_attach = function(client, bufnr)
        local common = require('plugins.lspconfig.common')
        common.set_mappings(client, bufnr, {
          ['K'] = {
            cmd = function()
              local actions = require('rust-quick-tests.hover_actions').get_hover_actions()
              if actions ~= nil then
                require('rust-quick-tests.hover_actions').show_actions(actions)
              else
                vim.lsp.buf.hover()
              end
            end,
            desc = '[Rust] Lsp Hover',
          },
          ['<leader>l'] = { cmd = ':RustLsp! runnables<CR>', desc = '[Rust] Run last runnable' },
          -- ['<leader>D'] = { cmd = ':RustLsp! debug<CR>', desc = '[Rust] Debug target under cursor' },
          ['<leader>m'] = { cmd = ':RustLsp! expandMacro<CR>', desc = '[Rust] Expand macros' },
        })
      end,

      default_settings = {
        ['rust-analyzer'] = {
          checkOnSave = true,
          procMacro = {
            enable = true,
            -- Don't expand some problematic proc_macros
            ignored = {
              ['async-trait'] = { 'async_trait' },
              ['napi-derive'] = { 'napi' },
              ['async-recursion'] = { 'async_recursion' },
              ['async-std'] = { 'async_std' },
            },
          },
          diagnostics = {
            enable = true,
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
  version = '^5',
  ft = { 'rust' },
}
