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
alias calc='emacs -f full-calc'
alias cp='xcp'
alias e='nvim'
alias find='fd'

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
  alias cat="COLORTERM=24bit bat --theme=$theme --style=changes,numbers"

  export BAT_THEME=$theme
  alias cap='cat -p'
fi


alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

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
elif command -v exa > /dev/null; then 
  alias ll='exa -lF --color-scale --no-user --no-time --no-permissions --group-directories-first --icons -a'
  alias ls='exa -lF --group-directories-first --icons -a'
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
alias s='kitty +kitten ssh'
alias get_idf='. ~/code/embedded/rust-build/export-esp.sh'
alias get_esp32='. ~/.config/zsh/export-esp32.sh'
alias get_esprs='. $HOME/export-esp.sh'
#alias get_idf='. $HOME/esp/esp-idf/export.sh'

# Load
autoload -U compinit; compinit

eval "$(direnv hook zsh)"

export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

if [ -x "$(command -v fzf)" ]; then
    # opensuse
    # source /etc/zsh_completion.d/fzf-key-bindings
    # arch
    # source /usr/share/fzf/completion.zsh
    # fedora
    source /usr/share/fzf/shell/key-bindings.zsh
fi

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -f "/home/jonatan/.ghcup/env" ] && source "/home/jonatan/.ghcup/env" # ghcup-env
export ZK_NOTEBOOK_DIR=$HOME/sources/zettels
alias python=python3

if [ "$USERNAME" = "user" ]; then 
  source $HOME/.asdf/asdf.sh
fi
