-- Opinionated Common Lisp dev tooling.
-- Every plugin is gated on `sbcl` being on PATH, so this module is inert
-- on machines without Common Lisp installed and never breaks startup.

local function sbcl_available()
  return vim.fn.executable("sbcl") == 1
end

vim.filetype.add({
  extension = {
    asd  = "lisp",
    cl   = "lisp",
    lsp  = "lisp",
    ros  = "lisp",
  },
  filename = {
    [".sbclrc"]  = "lisp",
    [".eclrc"]   = "lisp",
    [".clisprc"] = "lisp",
  },
})

vim.api.nvim_create_autocmd("FileType", {
  group   = vim.api.nvim_create_augroup("UserCommonLisp", { clear = true }),
  pattern = "lisp",
  callback = function()
    local o = vim.opt_local
    o.expandtab   = true
    o.tabstop     = 2
    o.shiftwidth  = 2
    o.softtabstop = 2
    o.lisp        = true
    o.iskeyword   = "@,48-57,_,192-255,-,*,+,!,?,:,/,<,>,=,&,|"
    o.commentstring = ";; %s"
  end,
})

if not sbcl_available() then
  return {}
end

vim.g.nvlime_config = vim.tbl_deep_extend("keep", vim.g.nvlime_config or {}, {
  leader = ",",
  server = {
    cl = { "sbcl", "--dynamic-space-size", "2048" },
  },
})

return {
  {
    "monkoose/nvlime",
    ft  = "lisp",
    dependencies = { "monkoose/parsley" },
    config = function()
      vim.api.nvim_create_user_command("NvlimeServer", function()
        vim.fn["nvlime#server#New"]()
      end, { desc = "Start a new Nvlime server and auto-connect" })
      vim.api.nvim_create_user_command("NvlimeConnect", function()
        vim.fn["nvlime#plugin#ConnectREPL"]()
      end, { desc = "Connect to an existing Nvlime/SWANK server" })
      vim.api.nvim_create_user_command("NvlimeStop", function()
        vim.fn["nvlime#plugin#StopCurrentServer"]()
      end, { desc = "Stop the current Nvlime server" })
    end,
  },

  {
    "julienvincent/nvim-paredit",
    ft = "lisp",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      use_default_keys = true,
      indent = { enabled = true },
    },
  },

  {
    "HiPhish/rainbow-delimiters.nvim",
    ft = "lisp",
  },
}
