# === Dev Environment Setup ===
[ -f /usr/share/fzf/shell/key-bindings.bash ] && source /usr/share/fzf/shell/key-bindings.bash

if [[ -z "$__ENV_LOADED" ]]; then 
    eval "$($HOME/.local/bin/load-env-from-1password.sh ReDevcontainer env-vars 2>/dev/null)" 2>/dev/null
    export __ENV_LOADED=1
fi

alias claude="claude --allow-dangerously-skip-permissions"
alias e="nvim"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$PATH:$HOME/.lmstudio/bin"

alias thoughts-init='thoughts init'

if [[ -n "$SSH_AUTH_SOCK" ]] && [[ "$SSH_AUTH_SOCK" != "$HOME/.ssh/ssh_auth_sock" ]]; then
    ln -sf "$SSH_AUTH_SOCK" "$HOME/.ssh/ssh_auth_sock"
    export SSH_AUTH_SOCK="$HOME/.ssh/ssh_auth_sock"
fi

alias ssh-fix='SSH_SOCK=$(find /tmp/ssh-* -name "agent.*" -user $USER 2>/dev/null | head -1) && \
  if [[ -n "$SSH_SOCK" ]]; then \
      ln -sf "$SSH_SOCK" "$HOME/.ssh/ssh_auth_sock" && \
      echo "Updated symlink to $SSH_SOCK"; \
  else \
      echo "No SSH agent socket found"; \
  fi'

alias tmux='ssh-fix && tmux new-session -A -s main'

# Auto-attach to tmux on SSH login (with SSH agent refresh)
if [[ -n "$SSH_CONNECTION" ]] && [[ -z "$TMUX" ]] && [[ $- == *i* ]] && command -v tmux &>/dev/null; then
    # Refresh SSH agent symlink before attaching so tmux sessions get the new socket
    SSH_SOCK=$(find /tmp/ssh-* -name "agent.*" -user $USER 2>/dev/null | head -1)
    if [[ -n "$SSH_SOCK" ]]; then
        ln -sf "$SSH_SOCK" "$HOME/.ssh/ssh_auth_sock"
    fi
    tmux new-session -A -s main
fi

[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"
[ -f "$HOME/.config/local/bin/env" ] && . "$HOME/.config/local/bin/env"


