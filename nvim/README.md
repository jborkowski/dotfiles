# Neovim Configuration

Lazy.nvim-based Neovim configuration with LSP support.

See [NEOVIM-0.12.md](NEOVIM-0.12.md) for the migration notes, native replacements,
key changes, retained-plugin rationale, and validation instructions.

## Requirements

- Neovim 0.12 or newer
- Git, curl, tar, and a C compiler
- `tree-sitter-cli` 0.26.1 or newer (required to build parsers)

On macOS with Homebrew:

```bash
brew install tree-sitter-cli
```

On other systems, install `tree-sitter-cli` with your OS package manager and verify it is available on `PATH`:

```bash
tree-sitter --version
```

Native Copilot completion also requires:

```bash
npm install --global @github/copilot-language-server
```

Then open Neovim and run `:Lazy sync` to install plugins and rebuild parsers.

## LSP Servers

### TypeScript / JavaScript

Uses `typescript-language-server` through nvim-lspconfig's `ts_ls` configuration.
`oxfmt` is also enabled as a formatting LSP when installed.

Install through the config's install-aware command:

```vim
:LspEnable ts_ls
:LspEnable oxfmt
```

Or install directly with Mason (`:LspInstall ts_ls`, `:LspInstall oxfmt`).

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

`<leader>` is Space and `<localleader>` is comma. Which-Key intentionally
shows only these two namespaces to avoid listing noisy builtin prefixes.

### Native LSP and diagnostics

| Key | Action |
|-----|--------|
| `gd` | Go to definition |
| `K` | Hover documentation |
| `gra` | Code action |
| `gri` | Implementation |
| `grn` | Rename symbol |
| `grr` | References |
| `grt` | Type definition |
| `grx` | Run code lens |
| `gO` | Document symbols |
| `<C-s>` in Insert mode | Signature help |
| `[d` / `]d` | Previous/next diagnostic |
| `[D` / `]D` | First/last diagnostic |
| `<C-w>d` | Diagnostic float |
| `<leader>cf` | Format document |
| `<leader>tf` | Toggle LSP formatting |
| `<leader>cd` | Diagnostics quickfix list |

### Leader groups

| Prefix | Group |
|--------|-------|
| `<leader>b` | Buffers |
| `<leader>c` | Code / LSP |
| `<leader>f` | Files |
| `<leader>g` | Git |
| `<leader>h` | Harpoon |
| `<leader>o` | Open |
| `<leader>p` | Project |
| `<leader>s` | Sessions |
| `<leader>t` | Toggle |
| `<leader>x` | Diagnostics |
| `<localleader>` (`,`) | Filetype-local commands |

## Commands

- `:LspEnable <server>` - Install through Mason when needed, then enable the LSP
- `:LspInstall <server>` - Install an LSP through Mason without enabling it
- `:LspInlayHints` - Toggle inlay hints
- `:LspToggleFormatting` - Toggle auto-formatting on save
