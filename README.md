# dotfiles

Personal configuration files managed with GNU Stow.

## Setup Flows

### Linux (Fedora)

Bootstrap system packages and apps (Silverblue only):
```bash
./silver-bootstrap
```

Install GTK theme and dependencies (Workstation/Silverblue):
```bash
./install.sh
```

Deploy dotfiles:
```bash
stow alacritty ghostty git nvim zsh sway kanshi systemd scripts gtk
```

### macOS

Install packages via Homebrew:
```bash
brew bundle
```

Deploy dotfiles:
```bash
stow alacritty ghostty git nvim zsh hammerspoon raycast karabiner
```

### Dev Container

See [re/README.md](re/README.md) for Restaumatic dev container customizations.

Unlock encrypted files:
```bash
git-crypt unlock
```

Deploy to dev container repo:
```bash
stow re -t /path/to/restaumatic-devcontainer-repo
```

## Structure

Dotfiles are organized by tool/application, each containing config files to be symlinked to `~`:

- `alacritty/`, `ghostty/`, `kitty/` - Terminal emulators
- `nvim/`, `helix/`, `doom/`, `emacs/`, `zed/` - Editors
- `sway/`, `kanshi/` - Wayland compositor (Linux)
- `hammerspoon/`, `karabiner/`, `raycast/` - macOS automation
- `kanata/`, `kmonad/` - Keyboard remapping
- `zsh/` - Shell configuration
- `git/` - Git configuration
- `scripts/` - Custom utilities
- `re/` - Dev container customizations (encrypted)

## Notes

- Encrypted files managed with git-crypt
- Use `stow` to selectively deploy configurations
- `re/` directory contains work-specific encrypted Docker configurations
