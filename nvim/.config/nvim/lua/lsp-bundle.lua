local M = {}

local bundle_path = vim.fn.stdpath("config") .. "/lsp-bundle.json"

local function get_registry()
  local ok, registry = pcall(require, "mason-registry")
  if not ok then
    vim.notify("Mason not installed", vim.log.levels.ERROR)
    return nil
  end
  return registry
end

function M.export()
  local registry = get_registry()
  if not registry then return end

  local packages = {}
  for _, pkg in ipairs(registry.get_installed_packages()) do
    packages[pkg.name] = pkg:get_installed_version() or "latest"
  end

  local file = io.open(bundle_path, "w")
  if not file then
    vim.notify("Failed to write bundle file", vim.log.levels.ERROR)
    return
  end

  file:write(vim.fn.json_encode(packages))
  file:close()

  local count = vim.tbl_count(packages)
  vim.notify(string.format("Exported %d packages to lsp-bundle.json", count), vim.log.levels.INFO)
end

function M.import()
  local registry = get_registry()
  if not registry then return end

  local file = io.open(bundle_path, "r")
  if not file then
    vim.notify("No bundle file found. Run :LspBundleExport first.", vim.log.levels.WARN)
    return
  end

  local content = file:read("*a")
  file:close()

  local ok, packages = pcall(vim.fn.json_decode, content)
  if not ok or not packages then
    vim.notify("Invalid bundle file", vim.log.levels.ERROR)
    return
  end

  local to_install = {}
  for name, _ in pairs(packages) do
    if not registry.is_installed(name) and registry.has_package(name) then
      table.insert(to_install, name)
    end
  end

  if #to_install == 0 then
    vim.notify("All packages already installed", vim.log.levels.INFO)
    return
  end

  vim.notify(string.format("Installing %d packages...", #to_install), vim.log.levels.INFO)

  for _, name in ipairs(to_install) do
    local pkg = registry.get_package(name)
    pkg:install()
  end
end

function M.list()
  local file = io.open(bundle_path, "r")
  if not file then
    vim.notify("No bundle file found", vim.log.levels.WARN)
    return
  end

  local content = file:read("*a")
  file:close()

  local ok, packages = pcall(vim.fn.json_decode, content)
  if not ok then return end

  local lines = { "Bundled LSP packages:" }
  for name, version in pairs(packages) do
    table.insert(lines, string.format("  %s (%s)", name, version))
  end
  table.sort(lines)
  vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO)
end

function M.setup()
  vim.api.nvim_create_user_command("LspBundleExport", M.export, {})
  vim.api.nvim_create_user_command("LspBundleImport", M.import, {})
  vim.api.nvim_create_user_command("LspBundleList", M.list, {})
end

return M
