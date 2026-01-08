return {
  "zbirenbaum/copilot.lua",
  cmd = "Copilot",
  event = "InsertEnter",
  config = function()
    -- Use mise to manage node for Copilot (requires Node 20+)
    local mise_path = vim.fn.expand("~/.local/bin/mise")

    -- Install mise if not available
    if vim.fn.executable(mise_path) == 0 then
      vim.notify("Installing mise for Copilot...", vim.log.levels.INFO)
      vim.fn.system("curl https://mise.run | sh")
    end

    -- Install node via mise (not global, just available)
    local node_path = vim.fn.expand("~/.local/share/mise/installs/node/22/bin/node")
    if vim.fn.executable(node_path) == 0 then
      vim.notify("Installing Node 22 via mise for Copilot...", vim.log.levels.INFO)
      -- Skip GPG verification to avoid permission issues
      vim.fn.system("MISE_NODE_VERIFY=0 " .. mise_path .. " install node@22")
      -- Find actual installed path
      local installs = vim.fn.glob(vim.fn.expand("~/.local/share/mise/installs/node/*/bin/node"), false, true)
      if #installs > 0 then
        node_path = installs[#installs] -- Use latest
      end
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
      copilot_node_command = node_path,
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
