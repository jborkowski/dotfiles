return {
  'rmagatti/auto-session',
  lazy = false,

  keys = function()
    return {
      { "<leader>ss", ":SessionSave<CR>",    desc = "Save Session" },
      { "<leader>sr", ":SessionRestore<CR>", desc = "Restore Session" },
    }
  end,

  ---enables autocomplete for opts
  ---@module "auto-session"
  ---@type AutoSession.Config
  opts = {
    suppressed_dirs = { '~/', '~/Downloads', '/' },
    root_dir = '~/.local/state/nvim/sessions/',
    auto_restore_enabled = false,
    auto_session_enabled = true,
    purge_after_minutes = 14400,

    session_lens = {
      load_on_setup = true,
      theme_conf = {
      },
      previewer = false,

      mappings = {
        delete_session = { "i", "<C-D>" },
        alternate_session = { "i", "<C-S>" },
        copy_session = { "i", "<C-Y>" },
      },

      session_control = {
        control_dir = '~/.local/state/nvim/auto_sessions/',
        control_filename = "session_control.json",
      },
    },
  }

}
