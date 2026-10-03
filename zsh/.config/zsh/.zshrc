# Created by Zap installer
[ -f "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh" ] && source "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh"
plug "zsh-users/zsh-autosuggestions"
plug "zap-zsh/supercharge"
plug "wintermi/zsh-lsd"
plug "zap-zsh/zap-prompt"
plug "zsh-users/zsh-syntax-highlighting"
plug "zsh-users/zsh-history-substring-search"
plug "marlonrichert/zsh-edit"
plug "hlissner/zsh-autopair"
plug "wintermi/zsh-mise"
plug "wintermi/zsh-gcloud"

# Prompt: show host name instead of the lightning icon.
# Example: (Solmigo) ➜ the-ai-research-log (! main)
PROMPT="%B%{$fg[blue]%}(%m) %(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )%{$fg[cyan]%}%c%{$reset_color%}"
PROMPT+="\$vcs_info_msg_0_ "

# Load and initialise completion system
autoload -Uz compinit
compinit

# Herdr is opt-in: run `herdr` to launch or attach to a session.

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
  # Refresh before each prompt so existing shells follow appearance changes.
  _sync_bat_theme() {
    local appearance
    if [[ "$OSTYPE" == darwin* ]]; then
      appearance=$(defaults read -globalDomain AppleInterfaceStyle 2>/dev/null)
      if [[ "$appearance" == Dark ]]; then
        export BAT_THEME=OneHalfDark
      else
        export BAT_THEME=OneHalfLight
      fi
    elif command -v dconf >/dev/null \
      && [[ $(dconf read /org/gnome/desktop/interface/color-scheme 2>/dev/null) == "'prefer-dark'" ]]; then
      export BAT_THEME=OneHalfDark
    else
      export BAT_THEME=ansi-light
    fi
  }
  autoload -Uz add-zsh-hook
  add-zsh-hook precmd _sync_bat_theme
  _sync_bat_theme
  # Let bat read BAT_THEME rather than freezing a --theme value in the alias.
  alias cat='COLORTERM=24bit bat --style=changes,numbers -p'
  alias cap='cat -p'
fi

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

# Pi
export PATH="/opt/homebrew/bin:$PATH"
