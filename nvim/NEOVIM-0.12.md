# Neovim 0.12 migration

This configuration targets Neovim 0.12 or newer. It intentionally prefers
Neovim built-ins when they provide the required behavior, while retaining
plugins that still add substantial functionality.

## Requirements

Install the Treesitter CLI before syncing plugins:

```bash
# macOS
brew install tree-sitter-cli

# verify on any platform (requires 0.26.1 or newer)
tree-sitter --version
```

Native Copilot inline completion uses GitHub's language server:

```bash
npm install --global @github/copilot-language-server
copilot-language-server --version
```

After installing dependencies, run `:Lazy sync`.

## Treesitter migration

`nvim-treesitter` was moved from the legacy `master` branch to its Neovim
0.12-only `main` branch. Parsers are built through `tree-sitter-cli` and
Neovim owns highlighting through `vim.treesitter.start()`.

This fixes the Markdown/injection decoration failure:

```text
Decoration provider "conceal_line": attempt to call method 'range'
```

The configured parser set includes Markdown, Go, PureScript, Rust, Lua, Zig,
and the other languages listed in `lua/plugins/treesitter.lua`.

## Native replacements

The following plugins were removed because Neovim 0.12 or an existing plugin
now covers their role:

| Removed | Replacement |
| --- | --- |
| `vim-commentary` | Native `gc` and `gcc` mappings |
| `lsp_signature.nvim` | Native signature help: `<C-s>` in Insert mode |
| `fidget.nvim` | Native progress messages and `vim.ui.progress_status()` in Lualine |
| `OSC11.nvim`, `themery.nvim` | Native terminal background detection plus Dayfox/Duskfox switching |
| `cobupilot-nvim` | Native `vim.lsp.inline_completion` with `copilot-language-server` |
| `nvim-navic` | Removed because it was attached but never displayed |
| `dressing.nvim` | Native UI plus Snacks input |
| `cmp-nvim-lua` | LuaLS completion through the normal LSP source |
| `FixCursorHold.nvim` | Unnecessary compatibility layer on Neovim 0.12 |
| `zig.vim` | Neovim Zig runtime, Treesitter, and ZLS |
| `go.nvim`, `guihua.lua` | Gopls, Treesitter, and LSP formatting |
| `cargo.nvim` | Rustaceanvim, Neotest, and terminal Cargo commands |
| disabled `fzf.vim` and `copilot.lua` specs | No runtime behavior |

### Native Copilot behavior

- `<Tab>` accepts an inline suggestion before falling through to `nvim-cmp`.
- `<M-]>` and `<M-[>` select the next/previous suggestion.
- Run `:LspCopilotSignIn` from a buffer inside a Git repository if authentication is required.
- The old Cobupilot panel and partial-acceptance features are intentionally not retained.

### Theme behavior

Neovim 0.12 detects the terminal background at startup and after the terminal
resumes. `lua/plugins/theme.lua` maps that value to:

- `dayfox` for a light background;
- `duskfox` for a dark background.

`<leader>tt` still toggles between light and dark themes.

## Install-aware LSP enabling

Neovim's lowercase `:lsp enable <name>` only enables a configuration; it does
not install the server executable. This config adds:

```vim
:LspEnable oxfmt
```

`:LspEnable <name>` resolves the matching Mason package, installs it when
missing, then toggles and re-enables the LSP so a previous spawn failure is
retried. `oxfmt` is registered in `lua/plugins/lspconfig.lua` and will enable
automatically on later starts once installed.

The standard Mason command `:LspInstall <name>` remains available when only
installation is desired.

## Native LSP mappings

The configuration keeps only custom mappings that are not supplied by Neovim.
Use the standard 0.12 mappings:

| Mapping | Action |
| --- | --- |
| `K` | Hover |
| `gra` | Code action |
| `gri` | Implementation |
| `grn` | Rename |
| `grr` | References |
| `grt` | Type definition |
| `grx` | Run code lens |
| `gO` | Document symbols |
| `<C-s>` in Insert mode | Signature help |
| `[d`, `]d` | Previous/next diagnostic |
| `[D`, `]D` | First/last diagnostic |
| `<C-w>d` | Diagnostic float |
| `an`, `in` in Visual mode | Expand/shrink semantic selection |

The local leader is comma (`vim.g.maplocalleader = ','`). Which-Key labels it
as **Local leader (,)**. Which-Key is intentionally restricted to leader and
local-leader triggers; builtin prefixes such as `g`, `c`, `v`, registers,
marks, and window commands are no longer dumped into its popup.

Redundant leader mappings were also removed: Telescope diagnostics and symbols
were duplicates of Trouble, Telescope's Harpoon marks duplicated Harpoon's
own UI, and the Netrw `:Explore` shortcut overlapped FFF/Telescope browsing.
All remaining leader mappings have descriptions, and `<leader>gL` labels the
nested GitHub-list commands.

Custom mappings retained:

- `gd`: definition
- `<leader>cf`: format
- `<leader>tf`: toggle LSP formatting
- `<leader>cd`: diagnostics quickfix list

## Performance changes

Heavy plugins are loaded only when needed:

- Telescope loads through its command or mappings.
- LSP and Mason load when editing files.
- Neotest and Rust tooling load for Rust.
- PureScript tooling loads for PureScript.
- Go and PureScript Treesitter parsers replace eager language plugins.
- Harpoon no longer loads while Lazy evaluates its key specifications.

A headless startup measurement on the migration machine dropped from roughly
323 ms to 64 ms. Interactive startup also schedules FFF intentionally when
Neovim starts without file arguments.

## Plugins intentionally retained

- `lazy.nvim`: dependency graph, lockfile, build hooks, and lazy loading are
  still more useful here than migrating to `vim.pack`.
- `nvim-cmp`: command-line, buffer, path, spell, calculator, and Nvlime sources
  are not a drop-in migration to native completion.
- `nvim-treesitter-textobjects`: semantic function/class/parameter selection,
  movement, and swapping exceed native node selection.
- Mason: Neovim configures LSP clients but does not install language servers.
- Conform: manages external formatters beyond native LSP formatting.
- Telescope and FFF: retain distinct specialized-search and fast-file-picker roles.
- AutoSession, Autopairs, Surround, Which-Key, Trouble, Harpoon, Neogit,
  Rustaceanvim, Neotest, and crates.nvim still provide non-native workflows.

## Useful Neovim 0.12 additions

- `:lsp` manages LSP clients interactively.
- `:Undotree` opens the built-in visual undo tree.
- `:DiffTool` compares files or directories.
- `:restart` or `ZR` restarts Neovim.
- Native incremental Treesitter/LSP selection uses `an`, `in`, `]n`, `[n`,
  `]N`, and `[N`.
- Experimental UI2 can be tried with
  `require('vim._core.ui2').enable()`, but is not enabled by this config.

## Validation

Quick checks used during the migration:

```bash
nvim --headless +qa
nvim --headless -c 'checkhealth lazy nvim-treesitter auto-session' \
  -c 'w! /tmp/nvim-health.txt' +qa
```

Markdown fenced-code conceal/injection, Lua/PureScript startup, Telescope lazy
loading, session commands, parser installation, and plugin health were also
smoke-tested without running the repository's slow test suite.
