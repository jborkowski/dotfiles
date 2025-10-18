#!/usr/bin/env bash

HOSTNAME=$(hostname -s)

cat > "$HOME/.dotfiles/agents-cli/.claude/settings.json" <<EOF
{
  "enabledPlugins": {
    "essentials@restaumatic-toolkit": true,
    "jonatan-toolkit@restaumatic-toolkit": true
  },
  "alwaysThinkingEnabled": false,
  "env": {
       "CLAUDE_CODE_ENABLE_TELEMETRY": "1",
       "OTEL_METRICS_EXPORTER": "otlp",
       "OTEL_LOGS_EXPORTER": "otlp",
       "OTEL_EXPORTER_OTLP_PROTOCOL": "grpc",
       "OTEL_EXPORTER_OTLP_ENDPOINT": "http://luddite:4317",
       "OTEL_SERVICE_NAME": "claude-${HOSTNAME}"
     }
}
EOF

cat > "$HOME/.dotfiles/agents-cli/.gemini/settings.json" <<EOF
{
  "security": {
    "auth": {
      "selectedType": "oauth-personal"
    }
  },
  "output": {
    "format": "json"
  },
  "ui": {
    "theme": "Shades Of Purple"
  },
  "telemetry": {
    "enabled": true,
    "target": "local",
    "otlpEndpoint": "http://luddite:4317"
  },
  "env": {
    "OTEL_SERVICE_NAME": "gemini-${HOSTNAME}"
  }
}
EOF

echo "Generated settings for hostname: ${HOSTNAME}"
