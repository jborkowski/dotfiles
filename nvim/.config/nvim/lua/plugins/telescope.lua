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

  dependencies = {
    'nvim-lua/plenary.nvim',
    { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
    'nvim-telescope/telescope-project.nvim',
    'nvim-telescope/telescope-file-browser.nvim',
    'nvim-telescope/telescope-github.nvim',
    'jborkowski/telescope-inflect.nvim'
  },

  enabled = true,
  lazy = false,

  keys = function()
    return {
      { "<leader>bb", builtin('buffers'),     desc = "Search buffers" },
      {
        '<Leader><leader>',
        extension('file_browser', 'file_browser', { path = '%:p:h', hidden = true }),
        desc = 'Browse files',
      },

      { "<leader>i",  builtin('diagnostics'), desc = "Telescope diagnostics" },
      {
        "<Leader>ff",
        builtin('find_files', {
          follow = true,
          hidden = true,
          hidden_files = true,
        }),
        desc = "Search files"
      },
      { "<leader>/",   extension('inflect', 'ripgrep'),             desc = "Inflect (ripgrep)" },
      { "<Leader>fm",  builtin('marks'),                            desc = 'Search marks' },
      { "<leader>hm",  extension('harpoon', 'marks'),               desc = "Harpoon marks" },
      { "<Leader>fo",  builtin('oldfiles'),                         desc = "Search recent files" },
      { "<Leader>fs",  builtin('grep_string'),                      desc = "Search from word under cursor" },
      { "<leader>pp",  extension('project', 'project'),             desc = "Telescope Project" },
      { "<Leader>fls", builtin('lsp_document_symbols'),             desc = "List lsp symbols for current buffer" },
      { "<leader>rr",  extension('refactoring', 'refactors'),       mode = 'v',                                  desc = 'Search refactors' },
      { "<leader>sl",  extension('session-lens', 'search_session'), desc = 'Search list' },

    }
  end,

  config = function(_, _)
    local project_actions = require("telescope._extensions.project.actions")
    local telescope = require("telescope")
    local previewers = require("telescope.previewers")
    local sorters = require("telescope.sorters")
    telescope.setup {
      defaults = {
        file_sorter = sorters.native_fzf_sorter,
        generic_sorter = sorters.native_fzf_sorter,
        file_ignore_patterns = { ".git/", ".cache", "%.pdf", ".stack-work/", "output/", "node_modules/", "target/", "out/", "dist/", "%.lock" },
        prompt_prefix = "  ",
        selection_caret = " ",
        path_display = { "smart" },
        -- grep_previewer = custom_cat_previewer,
        -- grep_previewer = previewers.cat.new
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
                table.insert(dirs, expanded_dir)
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
          },
        }
      },
      pickers = {
      },
    }


    telescope.load_extension('fzf')
    telescope.load_extension('file_browser')
    telescope.load_extension('project')
    telescope.load_extension('harpoon')
    telescope.load_extension('gh')
    telescope.load_extension("inflect")

    require('refactoring').setup({})
    telescope.load_extension('refactoring')

    vim.api.nvim_create_autocmd("User", {
      pattern = "TelescopeProjectSelected",
      callback = function()
        vim.cmd("SessionRestore")
      end,
    })

    vim.api.nvim_create_autocmd("User", {
      pattern = "TelescopeProjectSelected",
      callback = function()
        vim.g.auto_session_enabled = false
        vim.defer_fn(function()
          vim.g.auto_session_enabled = true
        end, 1000)
      end,
    })
  end,
}
