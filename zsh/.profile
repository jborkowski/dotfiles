# === Dev Environment Setup ===
[ -f /usr/share/fzf/shell/key-bindings.bash ] && source /usr/share/fzf/shell/key-bindings.bash

if [[ -z "$__ENV_LOADED" ]]; then 
    eval "$($HOME/.local/bin/load-env-from-1password.sh ReDevcontainer env-vars 2>/dev/null)" 2>/dev/null
    export __ENV_LOADED=1
fi

alias claude="claude --allow-dangerously-skip-permissions"
alias happy="happy --allow-dangerously-skip-permissions"
alias e="nvim"
export PATH="$HOME/.local/bin:$PATH"
export HAPPY_SERVER_URL=https://happy-server.lab.j14i.me

# Thoughts tool alias
alias thoughts-init='thoughts init'

# === SSH Agent Forwarding Fix for Tmux ===
# Create a stable symlink so tmux sessions always find the SSH agent
if [[ -n "$SSH_AUTH_SOCK" ]] && [[ "$SSH_AUTH_SOCK" != "$HOME/.ssh/ssh_auth_sock" ]]; then
    ln -sf "$SSH_AUTH_SOCK" "$HOME/.ssh/ssh_auth_sock"
    export SSH_AUTH_SOCK="$HOME/.ssh/ssh_auth_sock"
fi

# Alias to refresh SSH agent socket (run after reconnecting SSH)
alias ssh-fix='SSH_SOCK=$(find /tmp/ssh-* -name "agent.*" -user $USER 2>/dev/null | head -1) && \
  if [[ -n "$SSH_SOCK" ]]; then \
      ln -sf "$SSH_SOCK" "$HOME/.ssh/ssh_auth_sock" && \
      echo "Updated symlink to $SSH_SOCK"; \
  else \
      echo "No SSH agent socket found"; \
  fi'

# Auto-attach to tmux on SSH login
if [[ -n "$SSH_CONNECTION" ]] && [[ -z "$TMUX" ]] && [[ $- == *i* ]] && command -v tmux &>/dev/null; then
    tmux new-session -A -s main
fi
