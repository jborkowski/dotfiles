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
alias ssh-re='ssh -o SetEnv=OP_SERVICE_ACCOUNT_TOKEN="$(unset OP_SERVICE_ACCOUNT_TOKEN; op read op://Restaumatic/sa-token-devcontainer/credential)" re'



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
  alias ls='ls --color=auto --group-directories-first'
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

eval "$(direnv hook zsh)"

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
[ -f "$HOME/.config/local/bin/env" ] && source "$HOME/.config/local/bin/env"
[ -f "$HOME/.config/op/plugins.h" ] && source "$HOME/.config/op/plugins.h"

export ZK_NOTEBOOK_DIR=$HOME/sources/zettels
alias python=python3

if [ "$USERNAME" = "user" ]; then
  source $HOME/.asdf/asdf.sh
fi
export PATH="/opt/homebrew/sbin:/opt/homebrew/bin:$PATH"
export PATH="$HOME/.config/cache/.bun/bin:$PATH"

devcontainer() {
  if [ "$1" = "exec" ] && [ $# -ge 2 ]; then
 command devcontainer exec --workspace-folder . "$2" "${@:3}"
  else
    command devcontainer "$@" --workspace-folder .
  fi
}


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


## Only on my MBP16
[[ ! -f ~/sources/envs-injector/op-ssh-hook.plugin.zsh ]] || source ~/sources/envs-injector/op-ssh-hook.plugin.zsh

opencode() { op run --no-masking -- opencode "$@" }


alias wake_luddite="ssh admin@fd88::1 '/tool wol mac=BC:FC:E7:0A:67:88 interface=bridge'"
alias suspend_luddite="ssh luddite.local 'sudo systemctl suspend'"

export HAPPY_SERVER_URL=https://happy-server.lab.j14i.me
export CCS_PROXY_HOST=ccs.lab.j14i.me

alias claude='claude --allow-dangerously-skip-permissions'
alias ccs='ccs --allow-dangerously-skip-permissions'


# bun completions
[ -s "/Users/jonatan/.bun/_bun" ] && source "/Users/jonatan/.bun/_bun"
export PATH="$HOME/.bun/bin:$PATH"
alias init_personal_thoughts="humanlayer thoughts init --profile personal"


declare -A CCS_MODELS=(
  [ghcp]="gpt-5.2"
  [glm]="glm-4.7"
  [qwen]="qwen3-80b"
  [gpt]="gpt-5.2"
  [nemotron]="nemotron-30b"
)

# alias claude='claude --allow-dangerously-skip-permissions'

claude() {
  local CCS_PROXY_URL="https://ccs-proxy.lab.j14i.me"
  ## PUBLIC TOKEN - no security risk
  local CCS_PROXY_TOKEN="dc60333283c92bedd4009f3f87db35ad84e63e01ced4348340971c1e27aed487" 

  if [[ "$1" == "models" || "$1" == "list" ]]; then
    echo "Shortcuts:"
    echo "  ghcp     → claude-sonnet-4.5 (GitHub Copilot)"
    echo "  glm      → glm-4.7"
    echo "  qwen     → qwen3-80b (Hyperbolic)"
    echo "  gpt      → gpt-5.2 (GitHub Copilot)"
    echo "  nemotron → nemotron-30b (TokenFactory)"
    echo ""
    echo "All models from proxy:"
    curl -s "$CCS_PROXY_URL/v1/models" \
      -H "Authorization: Bearer $CCS_PROXY_TOKEN" 2>/dev/null \
      | jq -r '.data[].id' | sort | column
    return
  fi

  if [[ -n "${CCS_MODELS[$1]}" ]]; then
    local model="${CCS_MODELS[$1]}"
    shift
    ANTHROPIC_BASE_URL="$CCS_PROXY_URL" \
    ANTHROPIC_AUTH_TOKEN="$CCS_PROXY_TOKEN" \
    command claude --model "$model" "$@"
  elif [[ "$1" =~ ^(glm-|qwen|nemotron|gpt-|claude-sonnet-4\.5|claude-opus-4\.5|GLM-|Nemotron) ]]; then
    ANTHROPIC_BASE_URL="$CCS_PROXY_URL" \
    ANTHROPIC_AUTH_TOKEN="$CCS_PROXY_TOKEN" \
    command claude --model "$@" --allow-dangerously-skip-permissions
  else
    command claude "$@" --allow-dangerously-skip-permissions 
  fi
}
