# ~/.bash_profile - Bash login shell configuration

# XDG Base Directory Specification
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$XDG_CONFIG_HOME/local/share"
export XDG_CACHE_HOME="$XDG_CONFIG_HOME/cache"

# History configuration
export HISTFILE="$HOME/.bash_history"
export HISTSIZE=1000
export SAVEHIST=1000

# Default applications
export ALTERNATE_EDITOR=
export EDITOR="nvim"
export VISUAL="nvim"
export BROWSER="firefox"
export HOMEBREW_NO_ENV_HINTS=y

# Base PATH configuration
export PATH=$HOME/.cargo/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:$HOME/.local/bin:$HOME/.ghcup/bin:$PATH
export PATH=/opt/homebrew/bin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH
export PATH="$HOME/.emacs.d/.cache/lsp/lua-language-server/bin/":$PATH

# ESP32 toolchain paths
export PATH="$HOME/.espressif/tools/xtensa-esp32-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$HOME/.espressif/tools/xtensa-esp32s2-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$HOME/.espressif/tools/xtensa-esp32s3-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$PATH"

# TeX and other tools
export PATH="/Library/TeX/texbin:$PATH"
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export PATH="/opt/homebrew/opt/sphinx-doc/bin:$PATH"
export PATH="$HOME/.roswell/bin:$PATH"
export PATH="/Applications/kitty.app/Contents/MacOS:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"

# ESP32 development
export LIBCLANG_PATH="$HOME/.espressif/tools/xtensa-esp32-elf-clang/esp-15.0.0-20221014-aarch64-apple-darwin/esp-clang/lib/"

# Rust environment
[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"

# Terminal configuration
if [ -z "$TERM" ] || [ "$TERM" != "xterm-ghostty" ]; then
    export TERM=xterm-24bit
fi

# FZF configuration
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# ZK Notebook
export ZK_NOTEBOOK_DIR=$HOME/sources/zettels

# Source secret environment variables if they exist
[ -f "$HOME/.config/bash/.env.secret" ] && source "$HOME/.config/bash/.env.secret"

# Source .bashrc if it exists (for interactive shells)
if [ -f "$HOME/.config/bash/.bashrc" ]; then
    source "$HOME/.config/bash/.bashrc"
elif [ -f "$HOME/.bashrc" ]; then
    source "$HOME/.bashrc"
fi
