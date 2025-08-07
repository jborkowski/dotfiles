return {
  "yetone/avante.nvim",
  event = "VeryLazy",
  lazy = true,
  build = "make",
  version = false,
  opts = {
    provider = "hyperbolic",
    providers = {
      hyperbolic = {
        __inherited_from = "openai",
        api_key_name = "AVANTE_HYPERBOLIC_API_KEY",
        endpoint = "https://api.hyperbolic.xyz/v1",
        -- model = "deepseek-ai/DeepSeek-V3",
        model = "Qwen/Qwen3-Coder-480B-A35B-Instruct",
        extra_request_body = {
          temperature = 0.75,
          max_tokens = 512,
        },
      },
      gemini     = {
        endpoint = "https://generativelanguage.googleapis.com/v1beta/models/",
        model = "gemini-2.5-pro-preview-03-25",
        timeout = 30000
      },
      claude     = {
        endpoint = "https://api.anthropic.com",
        model = "claude-opus-4-1-20250805",
        timeout = 30000, -- Timeout in milliseconds
        model = "claude-opus-4-1-20250805",
        extra_request_body = {
          temperature = 0.75,
          max_tokens = 32000,
        },
      },
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
      wrap = true,        -- similar to vim.o.wrap
      width = 30,         -- default % based on available width
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
    "nvim-tree/nvim-web-devicons",
    "stevearc/dressing.nvim",
    "nvim-lua/plenary.nvim",
    "MunifTanjim/nui.nvim",
    "ibhagwan/fzf-lua",
    "stevearc/dressing.nvim",
    "zbirenbaum/copilot.lua",
    {
      'MeanderingProgrammer/render-markdown.nvim',
      opts = {
        file_types = { "markdown", "Avante" },
      },
      ft = { "markdown", "Avante" },
    },
  },
}
