require('nvim-treesitter.configs').setup {
  ensure_installed = {
	  "c",
	  "lua",
	  "vim",
	  "vimdoc",
	  "query",
	  "sql",
	  "rust",
	  "haskell",
	  "ruby",
	  "terraform",
	  "ocaml",
	  "dockerfile",
	  "yaml",
	  "json"
  },
  sync_install = false,
  auto_install = true,
  ignore_install = { "julia" },

  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
}
