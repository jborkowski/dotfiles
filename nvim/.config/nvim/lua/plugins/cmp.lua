return {
  "hrsh7th/nvim-cmp",
  event = "InsertEnter",

  -- event = {"InsertEnter", "CmdLineEnter"},
  -- event = {"InsertEnter"},
  -- event = {"BufReadPost", "BufNewFile"},

  dependencies = {
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-cmdline",
    "HiPhish/nvim-cmp-vlime",
  },

  enabled = true,
  opts = function(_, opts)
    local cmp = require("cmp")

    -- --------------------------------------------------------------------- }}}
    -- {{{ Confirmaiton options

    local confirm_opts = {
      -- behavior = cmp.ConfirmBehavior.Select,
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    }
    -- --------------------------------------------------------------------- }}}
    -- {{{ Add boarders to completion windows.

    local window = {
      completion = {
        border = "rounded",
        winhighlight = 'Normal:Pmenu,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:Pmenu',
        scrollbar = false,
      },
      documentation = {
        border = "rounded",
        winhighlight = 'Normal:Pmenu,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:Pmenu',
        scrollbar = false,
      },
    }

    -- --------------------------------------------------------------------- }}}
    -- {{{ Setup filetype and cmdline preferences.

    cmp.setup.filetype("gitcommit", {
      sources = cmp.config.sources({
        { name = "fugitive" },
      }, {
        { name = "buffer" },
        { name = "spell" },
      }),
    })

    cmp.setup.cmdline({ "/", "?" }, {
      mapping = cmp.mapping.preset.cmdline(),
      sources = {
        { name = "buffer" },
      },
    })

    cmp.setup.cmdline(":", {
      mapping = cmp.mapping.preset.cmdline(),
      sources = cmp.config.sources({
        { name = "path" },
      }, {
        { name = "cmdline", option = { ignore_cmds = { 'Man', "!'" } } },
      }),
    })

    -- --------------------------------------------------------------------- }}}
    -- {{{ Has words before

    -- Proper has_words_before for Copilot (handles empty lines correctly)
    local has_words_before = function()
      if vim.api.nvim_get_option_value("buftype", { buf = 0 }) == "prompt" then return false end
      local line, col = unpack(vim.api.nvim_win_get_cursor(0))
      return col ~= 0 and vim.api.nvim_buf_get_text(0, line - 1, 0, line - 1, col, {})[1]:match("^%s*$") == nil
    end

    -- --------------------------------------------------------------------- }}}
    -- {{{ lsp kind icons

    local kind_icons = {
      Array = " ",
      Boolean = " ",
      Calc = " ",
      Class = " ",
      Codium = "",
      Color = " ",
      Constant = " ",
      Constructor = "",
      Control = "",
      Copilot = " ",
      Enum = "",
      EnumMember = "",
      Event = "",
      Field = " ",
      File = " ",
      Folder = "󰉋 ",
      Function = " ",
      Interface = " ",
      Key = " ",
      Keyword = "",
      Method = " ",
      Module = "",
      Namespace = " ",
      Null = "󰟢 ",
      Number = " ",
      Object = " ",
      Operator = " ",
      Package = " ",
      Property = "",
      Reference = " ",
      Snippet = " ",
      Spell = " ",
      String = " ",
      Struct = " ",
      TabNine = " ",
      Text = " ",
      TypeParameter = " ",
      Unit = "",
      Value = " ",
      Variable = "β",
      Vsnip = " ",
      buffer = "",
      calc = " ",
      nvim_lsp = "",
      path = "",
      spell = " ",
      vsnip = " ",
    }
    -- --------------------------------------------------------------------- }}}
    -- {{{ Mappings

    local mapping = {
      ["<C-j>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
        else
          fallback()
        end
      end, { "i", "s" }),

      ["<C-k>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
        else
          fallback()
        end
      end, { "i", "s" }),

      ["<C-n>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
        else
          fallback()
        end
      end, { "i", "s" }),

      ["<C-p>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
        else
          fallback()
        end
      end, { "i", "s" }),

      ["<C-c>"] = cmp.mapping { i = cmp.mapping.abort(), c = cmp.mapping.close() },

      ["<CR>"] = cmp.mapping.confirm { select = true },

      ["<Tab>"] = vim.schedule_wrap(function(fallback)
        if cmp.visible() and has_words_before() then
          cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
        else
          fallback()
        end
      end),

      ["<S-Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
        else
          fallback()
        end
      end, { "i", "s" }),
    }

    -- --------------------------------------------------------------------- }}}
    -- {{{ Source mapping and formatting

    local source_mapping = {
      spell         = "[Spell]",
      buffer        = "[Buffer]",
      nvim_lsp      = "[LSP]",
      nvim_lua      = "[Lua]",
      path          = "[Path]",
      calc          = "[calc]",
      nvlime        = "[nvlime]",
    }

    local formatting = {
      fields = { 'kind', 'abbr', 'menu' },
      format = function(entry, vim_item)
        vim_item.kind = string.format('%s', kind_icons[vim_item.kind])
        vim_item.menu = (source_mapping)[entry.source.name]
        -- Truncate long completions to prevent display issues
        local maxwidth = 60
        if vim_item.abbr and #vim_item.abbr > maxwidth then
          vim_item.abbr = vim_item.abbr:sub(1, maxwidth) .. "…"
        end
        return vim_item
      end
    }

    -- --------------------------------------------------------------------- }}}
    -- {{{ Sources

    local sources = {
      { name = "nvim_lsp",      group_index = 1, keyword_length = 1, max_item_count = 10 },
      { name = "nvim_lua",      group_index = 1, keyword_length = 1, max_item_count = 10 },
      { name = "path",          group_index = 1, keyword_length = 3, max_item_count = 10 },
      { name = "buffer",        group_index = 2, keyword_length = 3, max_item_count = 5 },
      { name = "spell",         group_index = 2, keyword_length = 3, max_item_count = 5 },
      { name = "calc",          group_index = 2, keyword_length = 3, max_item_count = 5 },
      { name = "nvlime",        group_index = 1, keyword_length = 1, max_item_count = 10 },
    }

    -- --------------------------------------------------------------------- }}}
    -- {{{ Update the function argument opts with local choices made.

    opts.confirm_opts = confirm_opts
    opts.formatting = formatting
    opts.mapping = mapping
    opts.sources = sources
    opts.window = window

    -- --------------------------------------------------------------------- }}}
  end,

  config = function(_, opts)
    require("cmp").setup(opts)
  end,
}
