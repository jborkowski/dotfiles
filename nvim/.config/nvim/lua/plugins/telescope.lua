local builtin = function(fn, args)
  return function()
    pcall(require('telescope.builtin')[fn], args)
  end
end

local extension = function(name, fn, args)
  return function()
    pcall(require('telescope').extensions[name][fn], args)
  end
end

return {
  'nvim-telescope/telescope.nvim',
  -- tag = '0.1.8',

  dependencies = {
    'nvim-lua/plenary.nvim',
    { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
    'nvim-telescope/telescope-project.nvim',
    'nvim-telescope/telescope-file-browser.nvim',
    'fdschmidt93/telescope-egrepify.nvim',
    'nvim-telescope/telescope-github.nvim',
  },

  enabled = true,
  lazy = false,

  keys = function()
    return {
      { "<leader>fb", builtin('buffers'),     desc = "Search buffers" },
      {
        '<Leader>fe',
        extension('file_browser', 'file_browser', { path = '%:p:h', hidden = true }),
        desc = 'Browse files',
      },

      { "<leader>i",  builtin('diagnostics'), desc = "Telescope diagnostics" },
      -- { "<Leader>ff", builtin('find_files', { follow = true, hidden = true }), desc = "Search files" },
      {
        "<Leader>ff",
        builtin('find_files', {
          follow = true,
          hidden = true,
          hidden_files = true,
        }),
        desc = "Search files"
      },
      { "<Leader>/",   builtin('current_buffer_fuzzy_find'),  desc = 'Fuzzy file in file' },

      { "<leader>fg",  builtin('live_grep'),                  desc = "Live Grep (ripgrep)" },
      { "<Leader>fm",  builtin('marks'),                      desc = 'Search marks' },
      { "<leader>hm",  builtin('harpoon', 'marks'),           desc = "Harpoon marks" },

      { "<Leader>fo",  builtin('oldfiles'),                   desc = "Search recent files" },
      { "<Leader>fs",  builtin('grep_string'),                desc = "Search from word under cursor" },
      { "<leader>pp",  extension('project', 'project'),       desc = "Telescope Project" },
      { "<Leader>fls", builtin('lsp_document_symbols'),       desc = "List lsp symbols for current buffer" },
      { "<leader>rr",  extension('refactoring', 'refactors'), mode = 'v',                                  desc = 'Search refactors' },
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
          base_dirs = (function()
            local dirs = {}
            local m_dirs = {
              '~/sources',
              '~/code',
              '/workspace'
            }

            for _, dir in ipairs(m_dirs) do
              local expanded_dir = vim.fn.expand(dir)
              if vim.fn.isdirectory(expanded_dir) == 1 then
                table.insert(dirs, dir)
              end
            end

            return dirs
          end)(),
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
            },
          },
          pickers = {
            live_grep = {
              additional_args = function()
                return { '--hidden', '--follow' }
              end,
            },
          }
        }
      },
      defaults = {
        file_ignore_patterns = { ".git/", ".cache", "%.pdf", ".stack-work/", "output/", "node_modules/", "target" },
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
    telescope.load_extension('gh')

    require('refactoring').setup({})
    telescope.load_extension('refactoring')
  end,
}
