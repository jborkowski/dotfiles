#!/bin/bash
# ==============================================================================
#  Start Tailscale daemon and auto-connect if TS_AUTHKEY is set
#  This script runs at container startup
# ==============================================================================

# Start tailscaled daemon in the background if not already running
if ! pgrep -x tailscaled > /dev/null; then
    echo "🚀 Starting tailscaled daemon..." >&2
    # Try userspace networking if TUN device is not available
    if [ ! -e /dev/net/tun ]; then
        echo "⚠️  /dev/net/tun not available, using userspace networking" >&2
        sudo tailscaled --state=/var/lib/tailscale/tailscaled.state --socket=/var/run/tailscale/tailscaled.sock --tun=userspace-networking &
    else
        sudo tailscaled --state=/var/lib/tailscale/tailscaled.state --socket=/var/run/tailscale/tailscaled.sock &
    fi
    # Wait a bit for daemon to be ready
    sleep 2
fi

# Auto-connect if TS_AUTHKEY is provided
if [ -n "$TS_AUTHKEY" ]; then
    echo "🔗 TS_AUTHKEY found. Connecting to Tailscale..." >&2
    # Check if already connected
    if ! sudo tailscale status > /dev/null 2>&1; then
        sudo tailscale up --authkey="$TS_AUTHKEY" --accept-routes --hostname="${TS_HOSTNAME:-devcontainer}"
        echo "✅ Connected to Tailscale" >&2
    else
        echo "✅ Already connected to Tailscale" >&2
    fi
elif [ -n "$TS_STATE_ARG" ]; then
    # Alternative: restore from previous state
    echo "🔗 Restoring Tailscale connection..." >&2
    sudo tailscale up --accept-routes
else
    echo "ℹ️  Tailscale daemon started. Use 'tailscale up' to connect." >&2
fi
