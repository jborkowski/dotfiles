-- Auto-install LSP servers from Mason when opening files

local M = {}

-- Mapping of filetypes to Mason package names
local lsp_servers = {
  -- Languages
  python = "python-lsp-server",
  javascript = "typescript-language-server",
  typescript = "typescript-language-server",
  lua = "lua-language-server",
  go = "gopls",
  rust = "rust-analyzer",
  c = "clangd",
  cpp = "clangd",
  java = "jdtls",
  ruby = "solargraph",
  php = "intelephense",

  -- Web
  html = "html-lsp",
  css = "css-lsp",
  json = "json-lsp",
  yaml = "yaml-language-server",

  -- Shell & config
  bash = "bash-language-server",
  sh = "bash-language-server",
  zsh = "bash-language-server",

  -- Specialized
  haskell = "haskell-language-server",
  purescript = "purescript-language-server",
  zig = "zls",
  terraform = "terraform-ls",
  markdown = "marksman",

  -- Add more as needed
}

-- Check if a Mason package is installed
local function is_installed(package_name)
  local registry = require("mason-registry")
  return registry.is_installed(package_name)
end

-- Install a Mason package
local function install_package(package_name)
  local registry = require("mason-registry")

  if not registry.has_package(package_name) then
    vim.notify(
      string.format("Package '%s' not found in Mason registry", package_name),
      vim.log.levels.WARN
    )
    return
  end

  local pkg = registry.get_package(package_name)

  vim.notify(
    string.format("Installing %s...", package_name),
    vim.log.levels.INFO
  )

  pkg:install():once("closed", function()
    vim.notify(
      string.format("✓ %s installed! Restart nvim to use it.", package_name),
      vim.log.levels.INFO
    )
  end)
end

-- Check and prompt for LSP installation
function M.check_lsp(filetype)
  -- Skip if no LSP is defined for this filetype
  if not lsp_servers[filetype] then
    return
  end

  local package_name = lsp_servers[filetype]

  -- Check if already installed
  if is_installed(package_name) then
    return
  end

  -- Check if LSP is actually running (might be installed via other means)
  local clients = vim.lsp.get_clients({ bufnr = 0 })
  if #clients > 0 then
    return
  end

  -- Prompt user to install
  vim.schedule(function()
    local choice = vim.fn.confirm(
      string.format(
        "No LSP found for %s files.\n\nInstall %s from Mason?",
        filetype,
        package_name
      ),
      "&Yes\n&No\n&Never for " .. filetype,
      1
    )

    if choice == 1 then
      -- Install
      install_package(package_name)
    elseif choice == 3 then
      -- Never ask again for this filetype
      lsp_servers[filetype] = nil
      vim.notify(
        string.format("Won't ask about %s LSP again this session", filetype),
        vim.log.levels.INFO
      )
    end
  end)
end

-- Setup autocommand
function M.setup()
  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("AutoInstallLSP", { clear = true }),
    callback = function(args)
      -- Wait a bit to see if LSP attaches
      vim.defer_fn(function()
        M.check_lsp(args.match)
      end, 1000)
    end,
  })
end

return M
