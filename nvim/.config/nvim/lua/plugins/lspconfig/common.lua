local M = {}

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

-- Keep only mappings that Neovim 0.12 does not provide by default.
-- Native defaults include K, gra, gri, grn, grr, grt, grx, gO, [d, ]d,
-- <C-w>d, and <C-s> in Insert mode.
local default_lsp_mappings = {
  ['gd'] = { cmd = vim.lsp.buf.definition, desc = 'Go to definition' },
  ['<leader>cf'] = { cmd = vim.lsp.buf.format, desc = 'Format document' },
  ['<leader>tf'] = { cmd = ':LspToggleFormatting<CR>', desc = 'Toggle LSP formatting' },
  ['<leader>cd'] = { cmd = vim.diagnostic.setqflist, desc = 'Show diagnostics quickfix' },
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
end

return M
