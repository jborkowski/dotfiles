return {
  "zbirenbaum/copilot.lua",
  enabled = false, -- Temporarily disabled in favor of cobupilot-nvim
  cmd = "Copilot",
  event = "InsertEnter",
  config = function()
    local node_path = "node" -- Default to system node

    -- Check if system node exists and is version 20+
    local function get_node_major_version(path)
      local output = vim.fn.system(path .. " --version 2>/dev/null")
      local major = output:match("v(%d+)")
      return major and tonumber(major) or 0
    end

    local system_version = get_node_major_version("node")
    if system_version < 20 then
      -- System node is too old or missing, use mise
      local mise_path = vim.fn.expand("~/.local/bin/mise")

      -- Install mise if not available
      if vim.fn.executable(mise_path) == 0 then
        vim.notify("Installing mise for Copilot (Node 20+ required)...", vim.log.levels.INFO)
        vim.fn.system("curl https://mise.run | sh")
      end

      -- Check if node already installed via mise
      local installs = vim.fn.glob(vim.fn.expand("~/.local/share/mise/installs/node/*/bin/node"), false, true)
      if #installs > 0 then
        node_path = installs[#installs]
      else
        -- Install node via mise
        vim.notify("Installing Node 22 via mise for Copilot...", vim.log.levels.INFO)
        vim.fn.system("MISE_NODE_VERIFY=0 " .. mise_path .. " install node@22")
        installs = vim.fn.glob(vim.fn.expand("~/.local/share/mise/installs/node/*/bin/node"), false, true)
        if #installs > 0 then
          node_path = installs[#installs]
        end
      end
    end

    require("copilot").setup({
      suggestion = {
        enabled = true,
        auto_trigger = true,
        debounce = 50,
        keymap = {
          accept = false,
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

    vim.keymap.set("i", "<Tab>", function()
      if require("copilot.suggestion").is_visible() then
        require("copilot.suggestion").accept()
      else
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Tab>", true, false, true), "n", false)
      end
    end, { desc = "Accept Copilot or Tab" })
  end,
}
