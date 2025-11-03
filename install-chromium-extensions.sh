#!/bin/bash

# Script to install essential extensions for ungoogled Chromium
# Downloads CRX files and extracts them for "Load unpacked" installation

set -e

EXTENSIONS_DIR="$HOME/.chromium-extensions"
mkdir -p "$EXTENSIONS_DIR"
cd "$EXTENSIONS_DIR"

# Function to extract CRX file
extract_crx() {
  local crx_file="$1"
  local output_dir="$2"

  # Remove old directory if exists
  if [ -d "$output_dir" ]; then
    rm -rf "$output_dir"
  fi
  mkdir -p "$output_dir"

  # CRX files have a header we need to skip
  # Try to extract as ZIP (works for most CRX files)
  if unzip -q "$crx_file" -d "$output_dir" 2>/dev/null; then
    echo "    ✓ Extracted successfully"
  else
    # If that fails, skip the CRX header and try again
    # CRX3 format: skip first 16 bytes + header length
    python3 -c "
import sys, struct
with open('$crx_file', 'rb') as f:
    # Read magic number
    magic = f.read(4)
    if magic == b'Cr24':  # CRX3
        version = struct.unpack('<I', f.read(4))[0]
        header_size = struct.unpack('<I', f.read(4))[0]
        f.seek(12 + header_size)
    else:
        f.seek(0)

    # Write rest to temp ZIP
    with open('temp.zip', 'wb') as out:
        out.write(f.read())
" 2>/dev/null

    if [ -f "temp.zip" ]; then
      unzip -q temp.zip -d "$output_dir" 2>/dev/null || echo "    ⚠ Partial extraction"
      rm temp.zip
    fi
  fi

  rm "$crx_file"
}

echo "📦 Downloading extensions for ungoogled Chromium..."
echo ""

# Chrome Web Store enabler
echo "  → Chrome Web Store enabler..."
curl -L -o chromium-web-store.crx \
  "https://github.com/NeverDecaf/chromium-web-store/releases/latest/download/Chromium.Web.Store.crx"
extract_crx "chromium-web-store.crx" "chromium-web-store"

# Vimium - download from master branch (they don't do releases)
echo "  → Vimium..."
if [ -d "vimium" ]; then
  rm -rf vimium
fi

echo "    Downloading latest version from master..."
curl -L -o vimium-master.zip \
  "https://github.com/philc/vimium/archive/refs/heads/master.zip"
unzip -q vimium-master.zip
rm vimium-master.zip

# The extension files are in the root of the repo
mv "vimium-master" vimium
echo "    ✓ Downloaded successfully"

# uBlock Origin
echo "  → uBlock Origin..."
UBLOCK_VERSION=$(curl -s https://api.github.com/repos/gorhill/uBlock/releases/latest | grep '"tag_name"' | sed -E 's/.*"([^"]+)".*/\1/')
echo "    Downloading version $UBLOCK_VERSION..."
curl -L -o ublock.zip \
  "https://github.com/gorhill/uBlock/releases/download/${UBLOCK_VERSION}/uBlock0_${UBLOCK_VERSION}.chromium.zip"
if [ -d "ublock-origin" ]; then
  rm -rf ublock-origin
fi
unzip -q ublock.zip
# The extension is extracted to uBlock0.chromium, rename it
mv uBlock0.chromium ublock-origin
rm ublock.zip
echo "    ✓ Extracted successfully"

echo ""
echo "✅ Downloads complete!"
echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "📋 INSTALLATION INSTRUCTIONS"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "STEP 1: Enable Developer Mode"
echo "  1. Open ungoogled Chromium"
echo "  2. Navigate to: chrome://extensions/"
echo "  3. Toggle 'Developer mode' in the top-right corner"
echo ""
echo "STEP 2: Install extensions using 'Load unpacked'"
echo "  For each extension, click 'Load unpacked' and select the directory:"
echo ""
echo "  Available extensions:"
echo "  • $EXTENSIONS_DIR/chromium-web-store  (Chrome Web Store access)"
echo "  • $EXTENSIONS_DIR/vimium               (keyboard navigation)"
echo "  • $EXTENSIONS_DIR/ublock-origin        (ad blocker)"
echo ""
echo "STEP 3: Install from Chrome Web Store (after loading chromium-web-store)"
echo "  Visit these URLs to install additional extensions:"
echo "  • Bitwarden (password manager):"
echo "    https://chrome.google.com/webstore/detail/bitwarden/nngceckbapebfimnlniiiahkandclblb"
echo "  • Dark Reader (dark mode):"
echo "    https://chrome.google.com/webstore/detail/dark-reader/eimadpbcbfnmbkopoojfekhnkhdbieeh"
echo "  • Zotero Connector (research tool):"
echo "    https://chrome.google.com/webstore/detail/zotero-connector/ekhagklcjbdpajgpjgmbionohlpdbjgc"
echo ""
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "All extensions are in: $EXTENSIONS_DIR"
echo ""
