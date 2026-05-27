#!/usr/bin/env bash
# Build, sign, and install the `envkc` helper as a signed .app + PATH shim.
#
# TouchID-gated items live in the data-protection keychain, which AMFI only lets
# a binary touch if it carries an Apple-signed *provisioning profile* authorizing
# a <TeamID>.<group> keychain-access-group. A bare CLI can't embed a profile and
# a self-signed/Developer-ID-only signature gets SIGKILLed — all verified. So the
# binary must live inside an .app bundle (Contents/embedded.provisionprofile) and
# be signed with an "Apple Development" identity matching that profile.
#
# This script mints/refreshes the profile via Xcode automatic signing, embeds it,
# signs, and installs. Re-run it when the profile expires (~1 year) — that is the
# standing maintenance cost of the true ACL.
#
# Prerequisites (one-time):
#   • paid Apple Developer account, signed into Xcode (Settings ▸ Accounts)
#   • xcode-select -s /Applications/Xcode.app/Contents/Developer
#   • this Mac registered as a device in the account (Provisioning UDID below)
#   • xcodegen (brew install xcodegen)
set -euo pipefail
shopt -s nullglob

team="${ENVKC_TEAM:-ATQ45662TX}"
bundle_id="${ENVKC_BUNDLE_ID:-com.borkowski.envkc}"
group="${ENVKC_GROUP:-envkc}"
src="$(cd "$(dirname "${BASH_SOURCE[0]}")/envkc" && pwd)"
app="${ENVKC_APP_DIR:-$HOME/.config/local/EnvKC.app}"
shim="${ENVKC_BIN_DIR:-$HOME/.config/local/bin}/envkc"
udid="$(system_profiler SPHardwareDataType 2>/dev/null | awk -F': ' '/Provisioning UDID/{print $2}')"

devid="$(security find-identity -v -p codesigning | awk -F'"' '/Apple Development/{print $2; exit}')"

# Mint/refresh the team profile via Xcode automatic signing (best-effort; if it
# fails — offline, not signed in — we fall back to an existing valid profile).
mint() {
  command -v xcodegen >/dev/null || { echo "  (xcodegen missing; skipping mint)"; return 1; }
  local d; d="$(mktemp -d)"; mkdir -p "$d/App"
  echo 'print("mint")' >"$d/App/main.swift"
  cat >"$d/App/App.entitlements" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict><key>keychain-access-groups</key>
<array><string>\$(AppIdentifierPrefix)$group</string></array></dict></plist>
EOF
  cat >"$d/project.yml" <<EOF
name: EnvKCMint
options: { deploymentTarget: { macOS: "13.0" } }
targets:
  EnvKCMint:
    type: application
    platform: macOS
    sources: [App]
    settings: { base: {
      PRODUCT_BUNDLE_IDENTIFIER: $bundle_id, CODE_SIGN_STYLE: Automatic,
      DEVELOPMENT_TEAM: $team, CODE_SIGN_ENTITLEMENTS: App/App.entitlements,
      CODE_SIGN_IDENTITY: "Apple Development", GENERATE_INFOPLIST_FILE: YES } }
EOF
  ( cd "$d" && xcodegen generate >/dev/null 2>&1 \
      && xcodebuild -project EnvKCMint.xcodeproj -scheme EnvKCMint \
           -allowProvisioningUpdates -configuration Debug -derivedDataPath build \
           CODE_SIGN_STYLE=Automatic DEVELOPMENT_TEAM="$team" build >/dev/null 2>&1 )
  local rc=$?; rm -rf "$d"; return $rc
}

# Pick the newest non-expired profile covering <team>.<group> for this device.
find_profile() {
  local best="" best_m=0 dir p tmp groups exp now
  now="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  for dir in "$HOME/Library/Developer/Xcode/UserData/Provisioning Profiles" \
             "$HOME/Library/MobileDevice/Provisioning Profiles"; do
    for p in "$dir"/*.provisionprofile; do
      tmp="$(mktemp)"; security cms -D -i "$p" -o "$tmp" 2>/dev/null || { rm -f "$tmp"; continue; }
      groups="$(plutil -extract Entitlements.keychain-access-groups xml1 -o - "$tmp" 2>/dev/null || true)"
      grep -qE "$team\.(\*|$group)<" <<<"$groups" || { rm -f "$tmp"; continue; }
      grep -q "$udid" "$tmp" || { rm -f "$tmp"; continue; }   # device-scoped
      exp="$(plutil -extract ExpirationDate raw -o - "$tmp" 2>/dev/null || echo "")"
      rm -f "$tmp"
      [[ -n "$exp" && "$exp" > "$now" ]] || continue
      local m; m="$(stat -f %m "$p")"; (( m > best_m )) && { best="$p"; best_m="$m"; }
    done
  done
  [[ -n "$best" ]] && printf '%s' "$best"
}

[[ -n "$devid" ]] || { echo "✗ No 'Apple Development' identity. Sign into Xcode + build once." >&2; exit 1; }
echo "→ mint/refresh profile (team $team)…"; mint || echo "  (mint failed; using existing profile)"
profile="$(find_profile)" || true
[[ -n "$profile" ]] || { echo "✗ No valid profile covering $team.$group for device $udid." >&2
  echo "  Register this Mac (UDID $udid) and sign into Xcode, then re-run." >&2; exit 1; }
echo "→ profile: ${profile##*/}"

# Assemble + sign the .app, then install a PATH shim pointing into the bundle.
rm -rf "$app"; mkdir -p "$app/Contents/MacOS"
( cd "$src" && CGO_ENABLED=1 go build -trimpath -o "$app/Contents/MacOS/envkc" . )
cp "$profile" "$app/Contents/embedded.provisionprofile"
cat >"$app/Contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>CFBundleExecutable</key><string>envkc</string>
  <key>CFBundleIdentifier</key><string>$bundle_id</string>
  <key>CFBundleName</key><string>EnvKC</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleVersion</key><string>1</string>
  <key>CFBundleShortVersionString</key><string>1.0</string>
</dict></plist>
EOF
ent="$(mktemp)"; trap 'rm -f "$ent"' EXIT
cat >"$ent" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict><key>keychain-access-groups</key>
<array><string>$team.$group</string></array></dict></plist>
EOF
codesign --force --options runtime --sign "$devid" --entitlements "$ent" "$app"
mkdir -p "$(dirname "$shim")"; ln -sf "$app/Contents/MacOS/envkc" "$shim"

echo "✓ installed $app  (shim: $shim)"
codesign -dvv "$app" 2>&1 | grep -E 'Authority=Apple Development|TeamIdentifier' || true
echo "  test:  printf 's3kr3t' | envkc set -p demo -k DEMO_API_KEY && envkc get -p demo -k DEMO_API_KEY"
