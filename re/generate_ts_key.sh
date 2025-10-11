#!/bin/bash
# Generate ephemeral Tailscale auth key on host machine
# Usage: ./generate_ts_key.sh
# Requires: TS_OAUTH_CLIENT_ID and TS_OAUTH_CLIENT_SECRET environment variables

set -e

if [[ -z "$TS_OAUTH_CLIENT_ID" || -z "$TS_OAUTH_CLIENT_SECRET" ]]; then
    echo "❌ Error: TS_OAUTH_CLIENT_ID and TS_OAUTH_CLIENT_SECRET must be set" >&2
    echo "" >&2
    echo "To create OAuth credentials:" >&2
    echo "1. Go to https://login.tailscale.com/admin/settings/oauth" >&2
    echo "2. Create OAuth client with 'devices:write' scope" >&2
    echo "3. Set environment variables:" >&2
    echo "   export TS_OAUTH_CLIENT_ID='<client-id>'" >&2
    echo "   export TS_OAUTH_CLIENT_SECRET='<client-secret>'" >&2
    exit 1
fi

echo "🔑 Generating ephemeral Tailscale auth key..." >&2

# Get OAuth token
TOKEN=$(curl -s -d "client_id=${TS_OAUTH_CLIENT_ID}" \
                 -d "client_secret=${TS_OAUTH_CLIENT_SECRET}" \
                 "https://api.tailscale.com/api/v2/oauth/token" | \
        grep -o '"access_token":"[^"]*"' | cut -d'"' -f4)

if [[ -z "$TOKEN" ]]; then
    echo "❌ Failed to get OAuth token" >&2
    exit 1
fi

# Get tailnet from OAuth client ID
TAILNET="${TS_OAUTH_CLIENT_ID%%/*}"

# Generate ephemeral key (1 hour expiry)
AUTHKEY=$(curl -s -X POST \
               -H "Authorization: Bearer ${TOKEN}" \
               -H "Content-Type: application/json" \
               -d '{"capabilities":{"devices":{"create":{"reusable":false,"ephemeral":true,"preauthorized":true,"tags":["tag:devcontainer"]}}},"expirySeconds":3600}' \
               "https://api.tailscale.com/api/v2/tailnet/${TAILNET}/keys" | \
          grep -o '"key":"[^"]*"' | cut -d'"' -f4)

if [[ -z "$AUTHKEY" ]]; then
    echo "❌ Failed to generate ephemeral key" >&2
    exit 1
fi

echo "✅ Generated ephemeral auth key (expires in 1 hour)" >&2
echo "" >&2
echo "Add to docker-compose.local.yaml:" >&2
echo "  TS_AUTHKEY: \"$AUTHKEY\"" >&2
echo "" >&2
echo "Or export to shell:" >&2
echo "export TS_AUTHKEY=\"$AUTHKEY\""
