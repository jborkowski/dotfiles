return {
  'nvim-telescope/telescope.nvim', tag = '0.1.5',
    
  dependencies = { 'nvim-lua/plenary.nvim' },

  enabled = true,
  lazy = false,

  keys = function()
    local cmdT = "<cmd>Telescope "
    return {
      {"<leader>fC", cmdT .. "commands<cr>", desc = "Telescope commands" },
      {"<leader>fM", cmdT .. "man_pages<cr>", desc = "Telescope man pages" },
      {"<leader>fS", cmdT .. "colorscheme<cr>", desc = "Telescope colorschemes" },

      {"<leader>fb", cmdT .. "buffers<cr>", desc = "Telescope buffers" },
      {"<leader>fd", cmdT .. "diagnostics<cr>", desc = "Telescope diagnostics" },
      {"<leader>ff", cmdT .. "find_files<cr>", desc = "Telescope Find files" },
      {"<leader>fg", cmdT .. "live_grep<cr>", desc = "Telescope Live Grep" },
      {"<leader>fh", cmdT .. "help_tags<cr>", desc = "Telecope Help files" },

      {"<leader>fk", cmdT .. "keymaps<cr>", desc = "Telescope keymaps" },
      {"<leader>fl", cmdT .. "resume<cr>", desc = "Telescope resume" },
      {"<leader>fm", cmdT .. "marks<cr>", desc = "Telescope marks" },
      {"<leader>fo", cmdT .. "oldfiles<cr>", desc = "Telescope old files" },
      {"<leader>fp", cmdT .. "planets<cr>", desc = "Telescope Planets" },
      {"<leader>fw", cmdT .. "grep_string<cr>", desc = "" },

      {"<leader>gC", cmdT .. "git_commits<cr>", desc = "Telescope git commits" },
      {"<leader>gb", cmdT .. "git_branches<cr>", desc = "Telescope git branches" },
      {"<leader>go", cmdT .. "git_status<cr>", desc = "Telescope git status" },

      {"<leader>LS", cmdT .. "lsp_dynamic_workspace_symbols<cr>", desc = "Telescope Workspace Symbols" },
      {"<leader>Ls", cmdT .. "lsp_document_symbols<cr>", desc = "Telescope Document Symbols" },
    }
  end,

  config = function(_, opts)
    local telescope = require("telescope")
    telescope.setup(opts)
  end,
}


