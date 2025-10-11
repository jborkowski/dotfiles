#!/bin/sh
# ==============================================================================
#  Auto-connect to Tailscale if TS_AUTHKEY is set
# ==============================================================================
if [ -n "$TS_AUTHKEY" ]; then
  # Check if tailscale is installed and not already connected
  if command -v tailscale > /dev/null 2>&1; then
    # Check if already connected
    if ! tailscale status > /dev/null 2>&1; then
      echo "🔗 TS_AUTHKEY found. Connecting to Tailscale..." >&2
      # Use sudo because tailscaled needs root
      sudo tailscale up --authkey="$TS_AUTHKEY" --accept-routes
    fi
  else
    echo "⚠️  Warning: TS_AUTHKEY is set, but tailscale was not found." >&2
  fi
fi
