#!/bin/bash

# CONFIGURATION
BACKUP_FILE="$HOME/secure_gpg_backup/gpg_backup.tar.gz"
TMP_DIR="/tmp/gpg_import_$$"

# Create temp directory
mkdir -p "$TMP_DIR"

# Extract archive
tar xzf "$BACKUP_FILE" -C "$TMP_DIR"

# Import private and public keys
if [ -f "$TMP_DIR/private.gpg" ]; then
    gpg --import "$TMP_DIR/private.gpg"
else
    echo "Private key not found!"
fi

if [ -f "$TMP_DIR/public.gpg" ]; then
    gpg --import "$TMP_DIR/public.gpg"
fi

# Clean up
shred -u "$TMP_DIR/private.gpg" "$TMP_DIR/public.gpg"
rmdir "$TMP_DIR"

echo "✅ GPG keys imported successfully."

