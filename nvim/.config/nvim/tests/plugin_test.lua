local M = {}

local passed = 0
local failed = 0
local errors = {}

local function test(name, fn)
  local ok, err = pcall(fn)
  if ok then
    passed = passed + 1
    return true
  else
    failed = failed + 1
    table.insert(errors, { name = name, err = tostring(err) })
    return false
  end
end

function M.run()
  passed = 0
  failed = 0
  errors = {}

  local plugins = {
    { name = "lazy", module = "lazy" },
    { name = "snacks", module = "snacks" },
    { name = "lualine", module = "lualine" },
    { name = "treesitter", module = "nvim-treesitter" },
    { name = "nvim-web-devicons", module = "nvim-web-devicons" },
  }

  for _, plugin in ipairs(plugins) do
    test(plugin.name .. " loads", function()
      local ok = pcall(require, plugin.module)
      assert(ok)
    end)
  end

  test("lazy.nvim reports plugins", function()
    local lazy = require("lazy")
    local stats = lazy.stats()
    assert(stats.count > 0)
  end)

  test("lsp.config exists", function()
    assert(vim.lsp.config)
  end)

  return { passed = passed, failed = failed, errors = errors }
end

function M.print_results()
  local results = M.run()
  print("\n=== Plugin Tests ===")
  print(string.format("Passed: %d, Failed: %d", results.passed, results.failed))
  for _, e in ipairs(results.errors) do
    print("  ✗ " .. e.name .. ": " .. e.err)
  end
  return results.failed == 0
end

return M
