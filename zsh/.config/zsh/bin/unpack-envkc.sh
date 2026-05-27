#!/usr/bin/env bash
# Restore the pre-built, signed EnvKC.app from the repo tarball — no Xcode/Go
# needed. The embedded provisioning profile is device-locked, so this only works
# on the Mac that built it (UDID in the profile); on a new machine, rebuild with
# build-envkc.sh instead. The code signature survives tar, so we verify it after.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
archive="$here/EnvKC.app.tar.gz"
dest="${ENVKC_APP_DIR:-$HOME/.config/local/EnvKC.app}"
shim="${ENVKC_BIN_DIR:-$HOME/.config/local/bin}/envkc"

[[ -f "$archive" ]] || { echo "✗ $archive missing" >&2; exit 1; }

rm -rf "$dest"
mkdir -p "$(dirname "$dest")"
tar xzf "$archive" -C "$(dirname "$dest")"
mkdir -p "$(dirname "$shim")"; ln -sf "$dest/Contents/MacOS/envkc" "$shim"

if codesign --verify --strict "$dest" 2>/dev/null; then
  echo "✓ restored $dest  (shim: $shim) — signature valid"
else
  echo "✗ restored, but signature INVALID — rebuild with build-envkc.sh" >&2
  exit 1
fi
echo "  test:  printf 's3kr3t' | envkc set -p demo -k DEMO_API_KEY && envkc get -p demo -k DEMO_API_KEY"
