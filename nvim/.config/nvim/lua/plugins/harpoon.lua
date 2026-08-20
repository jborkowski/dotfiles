return {
  "ThePrimeagen/harpoon",
  branch = "harpoon2",
  dependencies = { "nvim-lua/plenary.nvim" },
  config = function()
    local harpoon = require('harpoon')
    harpoon.setup({})
  end,
  keys = {
    { "<leader>ha", function() require("harpoon"):list():add() end, desc = "Harpoon add" },
    { "<C-e>", function()
      local harpoon = require("harpoon")
      harpoon.ui:toggle_quick_menu(harpoon:list())
    end, desc = "Harpoon UI" },
    { "<C-S-N>", function() require("harpoon"):list():next() end, desc = "Harpoon Next Buffer" },
    { "<C-S-P>", function() require("harpoon"):list():prev() end, desc = "Harpoon Prev Buffer" },
  },
}
