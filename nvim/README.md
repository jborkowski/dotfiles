# Neovim Configuration

Lazy.nvim-based Neovim configuration with LSP support.

## LSP Servers

### TypeScript / JavaScript

Uses **vtsls** (formerly ts_ls), a performant TypeScript language server.

Install via npm:
```bash
npm install -g @vtsls/language-server
```

Or via Mason (`:MasonInstall vtsls`).

### Other LSPs

Configured LSPs are auto-installed via Mason when opening their filetypes. See `lua/auto-install-lsp.lua` for the full list.

Common ones:
- `lua_ls` - Lua
- `gopls` - Go
- `hls` - Haskell
- `clangd` - C/C++
- `pylsp` - Python
- `rust-analyzer` - Rust

## Key Bindings

| Key | Action |
|-----|--------|
| `gd` | Go to definition |
| `gf` | Show references (Telescope) |
| `K` | Hover documentation |
| `gt` | Type definition |
| `gr` | Rename symbol |
| `<leader>ca` | Code actions |
| `<leader>cf` | Format document |
| `<leader>cs` | Signature help |
| `[d` / `]d` | Previous/Next diagnostic |

## Commands

- `:LspInlayHints` - Toggle inlay hints
- `:LspToggleFormatting` - Toggle auto-formatting on save
