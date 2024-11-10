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
      { "<C-e>", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end, desc = "Harpoon UI" },
      { "<C-S-N>", function() harpoon:list():next() end, desc = "Harpoon Next Buffer" },
      { "<C-S-P>", function() harpoon:list():prev() end, desc = "Harpoon Prev Buffer" },
    }
  end
}
