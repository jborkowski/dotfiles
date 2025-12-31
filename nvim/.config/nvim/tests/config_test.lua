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

  test("utils module loads", function()
    local utils = require("utils")
    assert(utils.lua_command, "lua_command should exist")
  end)

  test("lspconfig common module loads", function()
    local common = require("plugins.lspconfig.common")
    assert(common.set_mappings, "set_mappings should exist")
  end)

  test("safe_enable logic works", function()
    local function safe_enable(_, cmd)
      if cmd and vim.fn.executable(cmd) == 0 then
        return false
      end
      return true
    end
    assert(safe_enable("test", nil) == true)
    assert(safe_enable("test", "nvim") == true)
    assert(safe_enable("test", "nonexistent_xyz_123") == false)
  end)

  test("leader key is space", function()
    assert(vim.g.mapleader == " ")
  end)

  test("localleader key is comma", function()
    assert(vim.g.maplocalleader == ",")
  end)

  test("clipboard is unnamedplus", function()
    local cb = vim.opt.clipboard:get()
    assert(cb[1] == "unnamedplus" or vim.tbl_contains(cb, "unnamedplus"))
  end)

  test("expandtab is enabled", function()
    assert(vim.o.expandtab == true)
  end)

  test("tabstop is 2", function()
    assert(vim.o.tabstop == 2)
  end)

  test("shiftwidth is 2", function()
    assert(vim.o.shiftwidth == 2)
  end)

  test("swapfile is disabled", function()
    assert(vim.opt.swapfile:get() == false)
  end)

  test("lazy.nvim is loaded", function()
    local ok = pcall(require, "lazy")
    assert(ok)
  end)

  test("cursor_colemak module loads", function()
    local ok = pcall(require, "cursor_colemak")
    assert(ok)
  end)

  return { passed = passed, failed = failed, errors = errors }
end

function M.print_results()
  local results = M.run()
  print("\n=== Config Tests ===")
  print(string.format("Passed: %d, Failed: %d", results.passed, results.failed))
  for _, e in ipairs(results.errors) do
    print("  ✗ " .. e.name .. ": " .. e.err)
  end
  return results.failed == 0
end

return M
