return {
  "zbirenbaum/copilot.lua",
  cmd = "Copilot",
  event = "InsertEnter",
  config = function()
    -- Install bun if not available (needed for Copilot on systems with old Node)
    local bun_path = vim.fn.expand("~/.bun/bin/bun")
    if vim.fn.executable(bun_path) == 0 then
      vim.notify("Installing bun for Copilot...", vim.log.levels.INFO)
      vim.fn.system("curl -fsSL https://bun.sh/install | bash")
    end

    require("copilot").setup({
      suggestion = {
        enabled = true,
        auto_trigger = true,
        debounce = 50,
        keymap = {
          accept = false, -- Handle Tab manually below
          accept_word = "<M-w>",
          accept_line = "<M-j>",
          next = "<M-]>",
          prev = "<M-[>",
          dismiss = "<C-]>",
        },
      },
      panel = { enabled = false },
      filetypes = {
        markdown = true,
        help = true,
      },
      copilot_node_command = vim.fn.expand("~/.bun/bin/bun"),
    })

    -- Tab to accept suggestion (like Zed), fallback to normal Tab
    vim.keymap.set("i", "<Tab>", function()
      if require("copilot.suggestion").is_visible() then
        require("copilot.suggestion").accept()
      else
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Tab>", true, false, true), "n", false)
      end
    end, { desc = "Accept Copilot or Tab" })
  end,
}
