return {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      local parsers = {
        "bash", "c", "cpp", "dockerfile", "html", "javascript", "json", "haskell",
        "lua", "markdown", "markdown_inline", "python", "query", "regex",
        "rust", "ruby", "sql", "toml", "tsx", "typescript", "vim", "vimdoc", "yaml", "zig"
      }

      vim.api.nvim_create_autocmd("VimEnter", {
        callback = function()
          vim.defer_fn(function()
            local ok, config = pcall(require, "nvim-treesitter.config")
            if not ok then return end
            local installed = config.get_installed()
            local to_install = vim.tbl_filter(function(p)
              return not vim.tbl_contains(installed, p)
            end, parsers)
            if #to_install > 0 then
              vim.notify("Installing " .. #to_install .. " treesitter parsers...", vim.log.levels.INFO)
              require("nvim-treesitter.install").install(to_install)
            end
          end, 100)
        end,
        once = true,
      })

      -- Enable treesitter highlighting and indentation
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
