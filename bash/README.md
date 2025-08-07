# Bash Configuration

This is a bash configuration setup that mirrors the functionality of the zsh configuration in this repository. It provides a modern, feature-rich bash environment with many of the same conveniences.

## Features

- **XDG Base Directory Specification compliant** - Keeps configuration in `~/.config/bash/`
- **Enhanced prompt** - Support for Starship (similar to Powerlevel10k)
- **Smart aliases** - Same aliases as the zsh config (ls, cat, find replacements, etc.)
- **Git integration** - Git status in prompt and helper functions
- **Auto-suggestions and syntax highlighting** - Via ble.sh
- **Fuzzy finder** - fzf integration
- **Better defaults** - Sensible bash settings
- **Directory navigation** - Enhanced cd with CDPATH
- **Colored output** - Colored man pages, ls, and completion

## Installation

1. **Copy the configuration files to your home directory:**
   ```bash
   # Copy the main config files
   cp -r .config/bash ~/.config/
   
   # Copy the root dotfiles
   cp .bashrc ~/
   cp .bash_profile ~/
   ```

2. **Install plugins and enhancements:**
   ```bash
   ~/.config/bash/install-plugins.sh
   ```

3. **Restart your terminal or source the configuration:**
   ```bash
   source ~/.bashrc
   ```

## File Structure

```
bash/
├── .bashrc                      # Root bashrc that sources XDG config
├── .bash_profile                # Login shell configuration with PATH and env vars
└── .config/
    └── bash/
        ├── .bashrc              # Main bash configuration
        ├── .env.secret          # Secret environment variables (not in git)
        ├── install-plugins.sh   # Plugin installation script
        └── functions/           # Bash functions
            └── git_prompt.sh    # Git prompt functions
```

## Key Differences from Zsh Configuration

While this bash configuration aims to provide similar functionality to the zsh setup, there are some differences:

### What's Similar
- All the same aliases (ls, cat, find, vim, etc.)
- PATH configuration
- Environment variables
- Git integration
- FZF support
- Direnv integration
- ESP32 development aliases

### What's Different
- **Prompt**: Uses Starship instead of Powerlevel10k (Powerlevel10k is zsh-only)
- **Auto-suggestions**: Uses ble.sh instead of zsh-autosuggestions
- **Syntax highlighting**: Also provided by ble.sh instead of fast-syntax-highlighting
- **History search**: Uses bash's built-in history search with arrow keys
- **Completion**: Uses bash-completion instead of zsh's compinit

### Bash-Specific Enhancements
- Colored man pages
- Better readline configuration
- CDPATH for quick directory navigation
- Enhanced history search with arrow keys
- Sensible bash defaults via bash-sensible plugin

## Customization

### Prompt
The prompt uses Starship by default. You can customize it by editing `~/.config/starship.toml`. If Starship is not installed, it falls back to a simple colored prompt.

### Aliases
All aliases are defined in `~/.config/bash/.bashrc`. You can add your own aliases there.

### Functions
Custom bash functions can be added to `~/.config/bash/functions/` and will be automatically sourced.

### Secret Environment Variables
Create `~/.config/bash/.env.secret` for environment variables you don't want in version control (API keys, tokens, etc.).

## Dependencies

The configuration works best with these tools installed:
- **starship** - Modern prompt
- **fzf** - Fuzzy finder
- **ble.sh** - Line editor with autosuggestions and syntax highlighting
- **bat** - Better cat with syntax highlighting
- **eza/exa/lsd** - Better ls with icons and colors
- **fd** - Better find
- **btm** - Better top
- **xcp** - Better cp
- **direnv** - Per-directory environment variables
- **nvim** - Neovim editor

Most of these will be installed automatically by the install script if they're not already present.

## Migrating from Zsh

If you're switching from zsh to bash, this configuration should feel familiar. The main things to be aware of:

1. **History**: Bash history is stored in `~/.bash_history` instead of `~/.config/zsh/.history`
2. **Completion**: Bash completion is less sophisticated than zsh, but bash-completion helps
3. **Prompt**: Starship provides a similar experience to Powerlevel10k
4. **Plugins**: The plugin ecosystem is smaller, but ble.sh provides many zsh-like features

## Troubleshooting

### Prompt not showing git information
Make sure bash-git-prompt is installed:
```bash
~/.config/bash/install-plugins.sh
```

### Auto-suggestions not working
Ensure ble.sh is properly installed:
```bash
cd ~/.config/bash/plugins/blesh
make install PREFIX="$HOME/.local"
```

### Colors not working
Check that your terminal supports 24-bit color:
```bash
echo $COLORTERM
```
Should output `truecolor` or `24bit`.

### Slow startup
If bash is starting slowly, you can disable some features:
- Comment out ble.sh sourcing in `.bashrc`
- Use a simpler prompt instead of Starship
- Reduce the number of plugins loaded

## License

This configuration is provided as-is for personal use. Feel free to modify and distribute as needed.