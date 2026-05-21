if [[ -o interactive ]] \
  && [[ -z "$TMUX" ]] \
  && [[ -z "$VSCODE_RESOLVING_ENVIRONMENT" ]] \
  && [[ "$TERM_PROGRAM" != "vscode" ]] \
  && [[ "$TERM_PROGRAM" != "cursor" ]] \
  && [[ "$TERM" != "dumb" ]] \
  && [[ -t 0 && -t 1 ]] \
  && command -v tmux >/dev/null; then
  exec tmux new-session -A -s main
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.config/zsh/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Options
setopt HIST_SAVE_NO_DUPS
setopt APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# Bindings
bindkey -e

# Aliases
alias :q='exit'
alias cp='xcp'
alias e='nvim'
alias find='fd'

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


if command  -v lsd > /dev/null; then 
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


alias ps='ps'
alias top='btm'
alias tree='tree -a -C'
alias vim='nvim'
alias ggc='git add . && git commit -m.'

# Load
autoload -U compinit; compinit

command -v direnv > /dev/null && eval "$(direnv hook zsh)"


# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -f "$HOME/.config/local/bin/env" ] && source "$HOME/.config/local/bin/env"
[ -f "$HOME/.config/op/plugins.h" ] && source "$HOME/.config/op/plugins.h"

alias python=python3

if [ "$USERNAME" = "user" ]; then
  source $HOME/.asdf/asdf.sh
fi
export PATH="/opt/homebrew/sbin:/opt/homebrew/bin:$PATH"
export PATH="$HOME/.config/cache/.bun/bin:$PATH"

command -v load-env-from-1password.sh >/dev/null && source <(load-env-from-1password.sh)

# Function to manually switch to forwarded SSH agent
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


[[ ! -f ~/sources/envs-injector/op-ssh-hook.plugin.zsh ]] || source ~/sources/envs-injector/op-ssh-hook.plugin.zsh

alias wake_luddite="ssh admin@fd88::1 '/tool wol mac=BC:FC:E7:0A:67:88 interface=bridge'"
alias suspend_luddite="ssh luddite.local 'sudo systemctl suspend'"

# Remote tmux: tm <host> [session-name]
tm() {
  local host="${1:-luddite}"
  local session="${2:-main}"
  ssh -t "$host" "tmux new -A -s $session"
}

[ -s "$HOME/.bun" ] && ( 
   export PATH="$HOME/.bun/bin:$PATH" &&
   source "$HOME/.bun/_bun"
 )
alias init_personal_thoughts="humanlayer thoughts init --profile personal"


claude () {
  command claude --allow-dangerously-skip-permissions
}

. "$HOME/.config/local/bin/env"

# Load custom functions
for f in "$HOME/.config/zsh/functions/"*.zsh; do
  [ -f "$f" ] && source "$f"
done
