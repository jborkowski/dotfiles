local M = {}

local u = require('utils')

-- lsp commands
u.lua_command('LspDef', 'vim.lsp.buf.definition()')
u.lua_command('LspFormatting', 'vim.lsp.buf.format()')
u.lua_command('LspCodeAction', 'vim.lsp.buf.code_action()')
u.lua_command('LspHover', 'vim.lsp.buf.hover()')
u.lua_command('LspRename', 'vim.lsp.buf.rename()')
u.lua_command('LspRefs', 'vim.lsp.buf.references()')
u.lua_command('LspTypeDef', 'vim.lsp.buf.type_definition()')
u.lua_command('LspImplementation', 'vim.lsp.buf.implementation()')
u.lua_command('LspDiagPrev', 'vim.diagnostic.goto_prev()')
u.lua_command('LspDiagNext', 'vim.diagnostic.goto_next()')
u.lua_command('LspDiagLine', 'vim.diagnostic.open_float()')
u.lua_command('LspSignatureHelp', 'vim.lsp.buf.signature_help()')
u.lua_command('LspDiagQuickfix', 'vim.diagnostic.setqflist()')


vim.api.nvim_create_user_command('LspInlayHints', function()
  local filter = { bufnr = vim.api.nvim_get_current_buf() }
  local current_setting = vim.lsp.inlay_hint.is_enabled(filter)
  vim.lsp.inlay_hint.enable(not current_setting, filter)
end, {})

vim.api.nvim_create_user_command('LspToggleFormatting', function()
  local buf = vim.api.nvim_get_current_buf()
  local clients = vim.lsp.get_clients({ bufnr = buf })

  for _, client in ipairs(clients) do
    client.server_capabilities.documentFormattingProvider =
        not client.server_capabilities.documentFormattingProvider
    client.server_capabilities.documentRangeFormattingProvider =
        not client.server_capabilities.documentRangeFormattingProvider

    local state = client.server_capabilities.documentFormattingProvider and "enabled" or "disabled"
    vim.notify(string.format("Formatting %s for %s", state, client.name), vim.log.levels.INFO)
  end
end, {})

local default_lsp_mappings = {
  ['gd'] = { cmd = ':LspDef<CR>', desc = 'Go to definition' },
  ['gf'] = { cmd = ':Telescope lsp_references<CR>', desc = 'Show references' },
  ['K'] = { cmd = ':LspHover<CR>', desc = 'Hover information' },
  ['gt'] = { cmd = ':LspTypeDef<CR>', desc = 'Type definition' },
  ['gr'] = { cmd = ':LspRename<CR>', desc = 'Rename symbol' },
  ['<leader>ca'] = { cmd = ':lua vim.lsp.buf.code_action()<CR>', desc = 'Display code actions' },
  ['<leader>cf'] = { cmd = ':LspFormatting<CR>', desc = 'Format document' },
  ['<leader>tf'] = { cmd = ':LspToggleFormatting<CR>', desc = 'Format document' },
  ['<leader>cs'] = { cmd = ':LspSignatureHelp<CR>', desc = 'Signature Help' },
  ['<leader>cd'] = { cmd = ':LspDiagQuickfix<CR>', desc = 'Show diagnostics QuickFix' },
  ['<leader>d'] = { cmd = ':LspDiagLine<CR>', desc = 'Go to previous diagnostic' },
  ['[d'] = { cmd = ':LspDiagPrev<CR>', desc = 'Go to previous diagnostic' },
  [']d'] = { cmd = ':LspDiagNext<CR>', desc = 'Go to next diagnostic' },
}

local lsp_buf_format_augroup = vim.api.nvim_create_augroup('lsp_buf_format', { clear = true })
M.format_on_save = function(client, bufnr)
  if client.server_capabilities.documentFormattingProvider then
    vim.api.nvim_create_autocmd('BufWritePre', {
      group = lsp_buf_format_augroup,
      buffer = bufnr,
      callback = function()
        vim.lsp.buf.format()
      end,
    })
  end
end

M.set_mappings = function(client, bufnr, nmap_mappings)
  local mappings = vim.tbl_extend('force', default_lsp_mappings, nmap_mappings or {})
  if client.server_capabilities.inlayHintProvider then
    mappings['gh'] = { cmd = ':LspInlayHints<CR>', desc = '[lsp] toggle inlay hints' }
  end
  for key, item in pairs(mappings) do
    vim.keymap.set('n', key, item.cmd, { buffer = bufnr, desc = item.desc, noremap = true, silent = true })
  end

  M.format_on_save(client, bufnr)
end

return M
