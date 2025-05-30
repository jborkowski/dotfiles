return {
  "epwalsh/obsidian.nvim",
  version = "*",
  lazy = true,
  ft = "markdown,org,norg",
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  cmd = {
    "ObsidianBacklinks",
    "ObsidianToday",
    "ObsidianLinks",
    "ObsidianNew",
    "ObsidianOpen",
    "ObsidianQuickSwitch",
    "ObsidianSearch",
  },
  opts = {
    workspaces = {
      {
        name = "personal",
        path = "~/org/Notes",
      },
    },
    open_notes_in = "current",
    daily_notes = {
      folder = "Daily",
      date_format = "%Y-%m-%d",
      alias_format = "%B %-d, %Y",
      -- Optional, if you want to automatically insert a template from your template directory like 'daily.md'
      template = nil
    },
    new_notes_location = "Inbox",
    follow_url_func = function(url)
      -- Open the URL in the default web browser.
      vim.fn.jobstart({ "open", url })
    end,
    note_id_func = function(title)
      local suffix = ""
      if title ~= nil then
        -- If title is given, transform it into valid file name.
        suffix = title:gsub(" ", "-"):gsub("[^A-Za-z0-9-]", ""):lower()
      else
        -- If title is nil, just add 4 random uppercase letters to the suffix.
        for _ = 1, 4 do
          suffix = suffix .. string.char(math.random(65, 90))
        end
      end
      return os.date('%Y%m%dT%H%M%S') .. "--" .. suffix
    end,
    mappings = {
      ["gf"] = {
        action = function()
          return require("obsidian").util.gf_passthrough()
        end,
        opts = { noremap = false, expr = true, buffer = true },
      },
    },
    ui = {
      enable = false,
      checkboxes = {
        [" "] = { char = "󰄱", hl_group = "ObsidianTodo" },
        ["x"] = { char = "", hl_group = "ObsidianDone" },
        [">"] = { char = "", hl_group = "ObsidianRightArrow" },
        ["~"] = { char = "󰰱", hl_group = "ObsidianTilde" },
        ["!"] = { char = "", hl_group = "ObsidianImportant" },
        [" "] = { char = "☐", hl_group = "ObsidianTodo" },
        ["x"] = { char = "✔", hl_group = "ObsidianDone" },
      },
      external_link_icon = { char = "", hl_group = "ObsidianExtLinkIcon" },
      bullets = { char = "• ", hl_group = "ObsidianBullet" },
      block_ids = { hl_group = "ObsidianBlockID" },
      hl_groups = {
        -- The options are passed directly to `vim.api.nvim_set_hl()`. See `:help nvim_set_hl`.
        ObsidianTodo = { bold = true, fg = "#f78c6c" },
        ObsidianDone = { bold = true, fg = "#89ddff" },
        ObsidianRightArrow = { bold = true, fg = "#f78c6c" },
        ObsidianTilde = { bold = true, fg = "#ff5370" },
        ObsidianImportant = { bold = true, fg = "#d73128" },
        ObsidianBullet = { bold = true, fg = "#89ddff" },
        ObsidianRefText = { underline = true, fg = "#c792ea" },
        ObsidianExtLinkIcon = { fg = "#c792ea" },
        ObsidianTag = { italic = true, fg = "#89ddff" },
        ObsidianBlockID = { italic = true, fg = "#89ddff" },
        ObsidianHighlightText = { bg = "#75662e" },
      },
    }
  },
  config = function(_, opts)
    require("obsidian").setup(opts)
    -- Set conceallevel for Obsidian syntax features only in markdown files within the vault
    vim.api.nvim_create_autocmd("FileType", {
      pattern = "markdown",
      callback = function()
        local client = require("obsidian").get_client()
        if client and client:vault_relative_path(vim.fn.expand("%:p")) then
          vim.opt_local.conceallevel = 2
        end
      end,
    })
    -- Set conceallevel for Obsidian syntax features
    vim.opt.conceallevel = 2
    local wk = require("which-key")

    wk.add({
      { "<leader>nb", "<cmd>ObsidianBacklinks<cr>",                              desc = "Backlinks" },
      { "<leader>nc", "<cmd>lua require('obsidian').util.toggle_checkbox()<cr>", desc = "Toggle Checkbox" },
      { "<leader>nl", "<cmd>ObsidianLinks<cr>",                                  desc = "Links" },
      { "<leader>nm", "<cmd>ObsidianTemplate<cr>",                               desc = "Template" },
      { "<leader>np", "<cmd>ObsidianPasteImg<cr>",                               desc = "Paste Image" },
      { "<leader>nq", "<cmd>ObsidianQuickSwitch<cr>",                            desc = "Quick Switch" },
      { "<leader>nt", "<cmd>ObsidianTags<cr>",                                   desc = "Tags" },
      { "<leader>nw", "<cmd>ObsidianWorkspace<cr>",                              desc = "Workspace" },
      { "<leader>ny", "<cmd>ObsidianYesterday<cr>",                              desc = "Yesterday's Note" },
      {
        mode = "v",
        { "<leader>n", group = "Notes" },
        {
          "<leader>ne",
          function()
            local title = vim.fn.input({ prompt = "Enter title (optional): " })
            vim.cmd("ObsidianExtractNote " .. title)
          end,
          desc = "Extract text into new note"
        },
        {
          "<leader>nl",
          function()
            vim.cmd("ObsidianLink")
          end,
          desc = "Link text to an existing note"
        },
        {
          "<leader>nn",
          function()
            vim.cmd("ObsidianLinkNew")
          end,
          desc = "Link text to a new note"
        },
      }
    })
  end
}
