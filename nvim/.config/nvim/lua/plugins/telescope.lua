return {
  'nvim-telescope/telescope.nvim',
  -- tag = '0.1.8',

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
      { "<leader>fS", cmdT .. "colorscheme<cr>", desc = "Telescope colorschemes" },

      { "<leader>fb", cmdT .. "buffers<cr>", desc = "Telescope buffers" },
      { "<leader>fd", cmdT .. "diagnostics<cr>", desc = "Telescope diagnostics" },
      { "<leader>ff", cmdT .. "find_files find_command=rg,--ignore,--hidden,--files prompt_prefix=🔍<cr>", desc = "Telescope Find files" },
      { "<leader>fg", cmdT .. "egrepify<cr>", desc = "Telescope Live Grep (ripgrep)" },
      { "<leader>fG", cmdT .. "live_grep<cr>", desc = "Telescope Live Grep" },
      { "<leader>fh", cmdT .. "help_tags<cr>", desc = "Telecope Help files" },

      { "<leader>fk", cmdT .. "keymaps<cr>", desc = "Telescope keymaps" },
      { "<leader>hm", cmdT .. "harpoon marks<cr>", desc = "Harpoon marks" },
      { "<leader>fo", cmdT .. "oldfiles<cr>", desc = "Telescope old files" },
      { "<leader>fw", cmdT .. "grep_string<cr>", desc = "" },

      { "<leader>pp", ":lua require'telescope'.extensions.project.project{}<CR>", desc = "Telescope Project" },

      { "<leader>LS", cmdT .. "lsp_dynamic_workspace_symbols<cr>", desc = "Telescope Workspace Symbols" },
      { "<leader>Ls", cmdT .. "lsp_document_symbols<cr>", desc = "Telescope Document Symbols" },
    }
  end,

  config = function(_, _)
    local project_actions = require("telescope._extensions.project.actions")
    local telescope = require("telescope")
    local previewers = require("telescope.previewers")
    telescope.setup {
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
          base_dirs = {
            '~/sources',
            '~/code',
            '/workspace'
          },
          mappings = {
            n = {
              ['d'] = project_actions.delete_project,
              ['r'] = project_actions.rename_project,
              ['c'] = project_actions.add_project,
              ['C'] = project_actions.add_project_cwd,
              ['f'] = project_actions.find_project_files,
              ['b'] = project_actions.browse_project_files,
              ['s'] = project_actions.search_in_project_files,
              ['R'] = project_actions.recent_project_files,
              ['w'] = project_actions.change_working_directory,
              ['o'] = project_actions.next_cd_scope,
            },
            i = {
              ['<c-d>'] = project_actions.delete_project,
              ['<c-v>'] = project_actions.rename_project,
              ['<c-a>'] = project_actions.add_project,
              ['<c-A>'] = project_actions.add_project_cwd,
              ['<c-f>'] = project_actions.find_project_files,
              ['<c-b>'] = project_actions.browse_project_files,
              ['<c-s>'] = project_actions.search_in_project_files,
              ['<c-r>'] = project_actions.recent_project_files,
              ['<c-l>'] = project_actions.change_working_directory,
              ['<c-o>'] = project_actions.next_cd_scope,
              ['<c-w>'] = project_actions.change_workspace,
            }
          }
        }
      },
      defaults = {
        file_ignore_patterns = { ".git/", ".cache", "%.pdf", ".stack-work/", "output/", "node_modules/", },
        file_previewer = previewers.cat.new,
        grep_previewer = previewers.cat.new,
        set_env = { ['COLORTERM'] = 'truecolor' },
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
