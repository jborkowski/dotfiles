return {
  "ThePrimeagen/harpoon",
  branch = "harpoon2",
  dependencies = { "nvim-lua/plenary.nvim" },
  config = function()
    local harpoon = require('harpoon')
    harpoon.setup({})
  end,
  keys = function()
    local harpoon = require("harpoon")
    return {
      { "<leader>ha", function() harpoon:list():add() end, desc = "Harpoon add" },
      { "<leader>hm", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end, desc = "Harpoon UI" },
      { "<leader>hn", function() harpoon:list():next() end, desc = "Harpoon Next Buffer" },
      { "<leader>hp", function() harpoon:list():prev() end, desc = "Harpoon Prev Buffer" },
    }
  end
}
