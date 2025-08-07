# ~/.bashrc - Root bash configuration file

# XDG Base Directory Specification
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$XDG_CONFIG_HOME/local/share"
export XDG_CACHE_HOME="$XDG_CONFIG_HOME/cache"

# Source the main bash configuration from XDG location
if [ -f "$XDG_CONFIG_HOME/bash/.bashrc" ]; then
    source "$XDG_CONFIG_HOME/bash/.bashrc"
fi
