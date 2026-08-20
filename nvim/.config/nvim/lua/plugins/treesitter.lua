local parsers = {
  "bash", "c", "cpp", "dockerfile", "go", "html", "javascript", "json", "haskell",
  "lua", "markdown", "markdown_inline", "purescript", "python", "query", "regex",
  "rust", "ruby", "sql", "swift", "toml", "tsx", "typescript", "vim", "vimdoc", "yaml", "zig",
}

if vim.fn.executable("sbcl") == 1 then
  table.insert(parsers, "commonlisp")
end

return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    lazy = false,
    build = function()
      local treesitter = require("nvim-treesitter")
      treesitter.install(parsers, { force = true }):wait(300000)
    end,
    config = function()
      require("nvim-treesitter").setup()

      -- Neovim 0.12 owns highlighting; nvim-treesitter supplies queries and indentation.
      vim.api.nvim_create_autocmd("FileType", {
        callback = function()
          if pcall(vim.treesitter.start) then
            vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
          end
        end,
      })
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    branch = "main",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    config = function()
      require("nvim-treesitter-textobjects").setup({
        select = { lookahead = true },
        move = { set_jumps = true },
      })

      local select = require("nvim-treesitter-textobjects.select")
      local move = require("nvim-treesitter-textobjects.move")
      local swap = require("nvim-treesitter-textobjects.swap")

      -- Select textobjects
      vim.keymap.set({ "x", "o" }, "af", function() select.select_textobject("@function.outer") end, { desc = "outer function" })
      vim.keymap.set({ "x", "o" }, "if", function() select.select_textobject("@function.inner") end, { desc = "inner function" })
      vim.keymap.set({ "x", "o" }, "ac", function() select.select_textobject("@class.outer") end, { desc = "outer class" })
      vim.keymap.set({ "x", "o" }, "ic", function() select.select_textobject("@class.inner") end, { desc = "inner class" })
      vim.keymap.set({ "x", "o" }, "aa", function() select.select_textobject("@parameter.outer") end, { desc = "outer parameter" })
      vim.keymap.set({ "x", "o" }, "ia", function() select.select_textobject("@parameter.inner") end, { desc = "inner parameter" })

      -- Move to next/prev function/class
      vim.keymap.set({ "n", "x", "o" }, "]m", function() move.goto_next_start("@function.outer") end, { desc = "next function start" })
      vim.keymap.set({ "n", "x", "o" }, "]M", function() move.goto_next_end("@function.outer") end, { desc = "next function end" })
      vim.keymap.set({ "n", "x", "o" }, "[m", function() move.goto_previous_start("@function.outer") end, { desc = "prev function start" })
      vim.keymap.set({ "n", "x", "o" }, "[M", function() move.goto_previous_end("@function.outer") end, { desc = "prev function end" })
      vim.keymap.set({ "n", "x", "o" }, "]]", function() move.goto_next_start("@class.outer") end, { desc = "next class start" })
      vim.keymap.set({ "n", "x", "o" }, "][", function() move.goto_next_end("@class.outer") end, { desc = "next class end" })
      vim.keymap.set({ "n", "x", "o" }, "[[", function() move.goto_previous_start("@class.outer") end, { desc = "prev class start" })
      vim.keymap.set({ "n", "x", "o" }, "[]", function() move.goto_previous_end("@class.outer") end, { desc = "prev class end" })

      -- Swap parameters
      vim.keymap.set("n", "<leader>a", function() swap.swap_next("@parameter.inner") end, { desc = "swap next param" })
      vim.keymap.set("n", "<leader>A", function() swap.swap_previous("@parameter.inner") end, { desc = "swap prev param" })
    end,
  },
}
