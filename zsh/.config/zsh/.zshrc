# Created by Zap installer
[ -f "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh" ] && source "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh"
plug "zsh-users/zsh-autosuggestions"
plug "zap-zsh/supercharge"
plug "zap-zsh/zap-prompt"
plug "zsh-users/zsh-syntax-highlighting"
plug "zsh-users/zsh-history-substring-search"
plug "marlonrichert/zsh-edit"
plug "hlissner/zsh-autopair"
plug "chisui/zsh-nix-shell"
plug "rkh/zsh-jj"
plug "wintermi/zsh-mise"

# Prompt: show host name instead of the lightning icon.
# Example: (Solmigo) ➜ the-ai-research-log (! main)
PROMPT="%B%{$fg[blue]%}(%m) %(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )%{$fg[cyan]%}%c%{$reset_color%}"
PROMPT+="\$vcs_info_msg_0_ "

# Load and initialise completion system
autoload -Uz compinit
compinit

# Auto-launch tmux (skip if `main` is already attached elsewhere)
if [[ -o interactive ]] \
  && [[ -z "$TMUX" ]] \
  && [[ -z "$VSCODE_RESOLVING_ENVIRONMENT" ]] \
  && [[ "$TERM_PROGRAM" != "vscode" ]] \
  && [[ "$TERM_PROGRAM" != "cursor" ]] \
  && [[ "$TERM" != "dumb" ]] \
  && [[ -t 0 && -t 1 ]] \
  && command -v tmux >/dev/null \
  && ! tmux list-clients -t main 2>/dev/null | grep -q .; then
  exec tmux new-session -A -s main
fi

# ── Options ────────────────────────────────────────────
setopt HIST_SAVE_NO_DUPS
setopt APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# ── Bindings ───────────────────────────────────────────
bindkey -e

# ── Aliases ────────────────────────────────────────────
alias :q='exit'
alias cp='xcp'
alias e='nvim'
alias find='fd'
alias python=python3
alias ps='ps'
alias vim='nvim'
alias init_personal_thoughts="humanlayer thoughts init --profile personal"
alias wake_luddite="ssh admin@fd88::1 '/tool wol mac=BC:FC:E7:0A:67:88 interface=bridge'"
alias suspend_luddite="ssh luddite.local 'sudo systemctl suspend'"
alias nr='_nr'

if [[ -f "$HOME/.config/cache/.bun/bin/pi" ]]; then
  alias pi="bun $HOME/.config/cache/.bun/bin/pi"
fi

if command -v btm > /dev/null; then
  alias top='btm'
fi

if command -v bat > /dev/null; then
  light_theme="OneHalfLight"
  dark_theme="OneHalfDark"
  theme=$light_theme
  if uname -a | grep -q "Darwin"; then
    theme=$(defaults read -globalDomain AppleInterfaceStyle &> /dev/null && echo $dark_theme || echo $light_theme)
  elif [[ $(dconf read /org/gnome/desktop/interface/color-scheme) == "'prefer-dark'" ]]; then
    theme=$dark_theme
  else
    theme=ansi-light
  fi
  alias cat="COLORTERM=24bit bat --theme=$theme --style=changes,numbers -p"
  export BAT_THEME=$theme
  alias cap='cat -p'
fi

if command -v lsd > /dev/null; then
  alias ls='lsd'
  alias l='lsd -l'
  alias ll='lsd -la'
elif command -v eza > /dev/null; then
  alias eza='eza --group-directories-first'
  alias ls='eza'
  alias l='eza -a'
  alias la='eza -laF'
  alias ll='eza -lF'
  alias tree='eza --tree'
else
  alias ls='ls -G'
  alias l='ls -A'
  alias la='ls -lAF'
  alias ll='ls -lF'
fi

alias tree='tree -a -C'

# ── Source ─────────────────────────────────────────────
command -v direnv > /dev/null && eval "$(direnv hook zsh)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f "$HOME/.config/local/bin/env" ] && source "$HOME/.config/local/bin/env"
[ -f "$HOME/.config/op/plugins.h" ] && source "$HOME/.config/op/plugins.h"
[[ ! -f ~/sources/envs-injector/op-ssh-hook.plugin.zsh ]] || source ~/sources/envs-injector/op-ssh-hook.plugin.zsh

# command -v load-env-from-1password.sh >/dev/null && source <(load-env-from-1password.sh)

[ -s "$HOME/.bun" ] && (
   export PATH="$HOME/.bun/bin:$PATH" &&
   source "$HOME/.bun/_bun"
 )

# ── Custom functions ───────────────────────────────────
for f in "$HOME/.config/zsh/functions/"*.zsh; do
  [ -f "$f" ] && source "$f"
done

# ── Shell functions ────────────────────────────────────
use-forwarded-agent() {
    if [ -n "$SSH_CONNECTION" ]; then
        local forwarded_sock=$(find /tmp/ssh-* -name "agent.*" 2>/dev/null | head -1)
        if [ -n "$forwarded_sock" ]; then
            export SSH_AUTH_SOCK="$forwarded_sock"
            echo "Using forwarded agent: $SSH_AUTH_SOCK"
            ssh-add -l
        else
            echo "No forwarded agent socket found. Did you connect with 'ssh -A'?"
        fi
    else
        echo "Not in an SSH session"
    fi
}

claude () {
  command claude --allow-dangerously-skip-permissions "$@";
}
