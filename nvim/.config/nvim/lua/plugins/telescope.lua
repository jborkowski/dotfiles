return {
  'nvim-telescope/telescope.nvim',
  tag = '0.1.5',

  dependencies = {
    'nvim-lua/plenary.nvim',
    { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
    'nvim-telescope/telescope-project.nvim',
    'nvim-telescope/telescope-file-browser.nvim',
    'fdschmidt93/telescope-egrepify.nvim',
  },

  enabled = true,
  lazy = false,

  keys = function()
    local cmdT = "<cmd>Telescope "
    return {
      { "<leader>fC", cmdT .. "commands<cr>", desc = "Telescope commands" },
      { "<leader>fM", cmdT .. "man_pages<cr>", desc = "Telescope man pages" },
      { "<leader>fS", cmdT .. "colorscheme<cr>", desc = "Telescope colorschemes" },

      { "<leader>fb", cmdT .. "buffers<cr>", desc = "Telescope buffers" },
      { "<leader>fd", cmdT .. "diagnostics<cr>", desc = "Telescope diagnostics" },
      { "<leader>ff", cmdT .. "find_files find_command=rg,--ignore,--hidden,--files prompt_prefix=🔍<cr>", desc = "Telescope Find files" },
      { "<leader>fg", cmdT .. "egrepify<cr>", desc = "Telescope Live Grep (ripgrep)" },
      { "<leader>fG", cmdT .. "live_grep<cr>", desc = "Telescope Live Grep" },
      { "<leader>fh", cmdT .. "help_tags<cr>", desc = "Telecope Help files" },

      { "<leader>fk", cmdT .. "keymaps<cr>", desc = "Telescope keymaps" },
      { "<leader>fl", cmdT .. "resume<cr>", desc = "Telescope resume" },
      { "<leader>fm", cmdT .. "harpoon marks<cr>", desc = "Harpoon marks" },
      { "<leader>fo", cmdT .. "oldfiles<cr>", desc = "Telescope old files" },
      { "<leader>fp", cmdT .. "planets<cr>", desc = "Telescope Planets" },
      { "<leader>fw", cmdT .. "grep_string<cr>", desc = "" },

      { "<leader>pp", cmdT .. "project<cr>", desc = "Telescope Project" },

      { "<leader>LS", cmdT .. "lsp_dynamic_workspace_symbols<cr>", desc = "Telescope Workspace Symbols" },
      { "<leader>Ls", cmdT .. "lsp_document_symbols<cr>", desc = "Telescope Document Symbols" },
    }
  end,

  config = function(_, _)
    local telescope = require("telescope")
    local previewers = require("telescope.previewers")
    telescope.setup {
      defaults = {
        set_env = { ['COLORTERM'] = 'truecolor' },
      },
      extensions = {
        fzf = {
          fuzzy = true,
          override_generic_sorter = true,
          override_file_sorter = true,
          case_mode = "smart_case",
        },
        project = {
          theme = "dropdown",
          hidden_files = true,
          order_by = "recent",
          search_by = "title",
        }
      },
      defaults = {
        file_ignore_patterns = { ".git/", ".cache", "%.pdf", ".stack-work/", "output/", "node_modules/",  },
        file_previewer = previewers.cat.new,
        grep_previewer = previewers.cat.new,
      },
      set_env = {
        BAT_STYLE = "numbers,changes",
        COLORTERM = "24bit",
      },
    }
    telescope.load_extension('fzf')
    telescope.load_extension('file_browser')
    telescope.load_extension('project')
    telescope.load_extension('harpoon')
  end,
}
