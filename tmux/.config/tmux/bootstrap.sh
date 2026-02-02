#!/usr/bin/env bash
# Bootstrap tmux configuration
set -e

TPM_DIR="$HOME/.tmux/plugins/tpm"

if [ -d "$TPM_DIR" ]; then
  echo "TPM already installed at $TPM_DIR"
else
  echo "Installing TPM..."
  git clone https://github.com/tmux-plugins/tpm "$TPM_DIR"
  echo "TPM installed"
fi

echo ""
echo "Next steps:"
echo "  1. Start tmux: tmux"
echo "  2. Install plugins: Ctrl-a I"
