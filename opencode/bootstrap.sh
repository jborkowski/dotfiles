#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "==> Creating symlinks..."

# Remove existing targets if they exist (but not if they're already correct symlinks)
for target in ~/.config/opencode ~/.opencode; do
    if [ -e "$target" ] && [ ! -L "$target" ]; then
        echo "    Backing up existing $target to ${target}.backup"
        mv "$target" "${target}.backup"
    elif [ -L "$target" ]; then
        rm "$target"
    fi
done

# Create symlinks
ln -sf "$SCRIPT_DIR/.config/opencode" ~/.config/opencode
ln -sf "$SCRIPT_DIR/.opencode" ~/.opencode

echo "==> Installing dependencies..."
cd ~/.config/opencode
npm install

echo "==> OpenCode setup complete!"
echo ""
echo "Symlinks created:"
echo "  ~/.config/opencode -> $SCRIPT_DIR/.config/opencode"
echo "  ~/.opencode -> $SCRIPT_DIR/.opencode"
