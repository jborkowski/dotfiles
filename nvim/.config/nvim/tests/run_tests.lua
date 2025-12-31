vim.opt.rtp:prepend(vim.fn.stdpath("config"))

local config_ok = require("tests.config_test").print_results()
local plugin_ok = require("tests.plugin_test").print_results()

print("\n=== Final Summary ===")
if config_ok and plugin_ok then
  print("All tests passed!")
  vim.cmd("qa!")
else
  print("Some tests failed!")
  vim.cmd("cq!")
end
