return {
  "yetone/avante.nvim",
  event = "VeryLazy",
  build = "make",
  opts = {
    ---@alias Provider "openai" | "claude" | "azure"  | "copilot" | "cohere" | [string]
    provider = "claude",
    claude = {
      endpoint = "https://api.anthropic.com",
      model = "claude-3-5-sonnet-latest",
      temperature = 0,
      max_tokens = 4096,
    },
    mappings = {
      ask = "<leader>aa",
      edit = "<leader>ae",
      --- @class AvanteConflictMappings
      diff = {
        ours = "co",
        theirs = "ct",
        both = "cb",
        next = "]x",
        prev = "[x",
      },
      jump = {
        next = "]]",
        prev = "[[",
      },
      submit = {
        normal = "<CR>",
        insert = "<C-s>",
      },
    },
    hints = { enabled = true },
    windows = {
      wrap = true, -- similar to vim.o.wrap
      width = 30, -- default % based on available width
      sidebar_header = {
        align = "center", -- left, center, right for title
        rounded = true,
      },
    },
    highlights = {
      ---@type AvanteConflictHighlights
      diff = {
        current = "DiffText",
        incoming = "DiffAdd",
      },
    },
    --- @class AvanteConflictUserConfig
    diff = {
      debug = false,
      autojump = true,
      ---@type string | fun(): any
      list_opener = "copen",
    },
    -- add any opts here
  },
  dependencies = {
    "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
    "stevearc/dressing.nvim",
    "nvim-lua/plenary.nvim",
    "MunifTanjim/nui.nvim",
    {
      'MeanderingProgrammer/render-markdown.nvim',
      opts = {
        file_types = { "markdown", "Avante" },
      },
      ft = { "markdown", "Avante" },
    },
  },
}
