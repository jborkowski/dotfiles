# ~/.config/bash/.bashrc - Bash configuration analogous to zsh setup

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# History configuration
export HISTSIZE=1000
export HISTFILESIZE=2000
export HISTCONTROL=ignoredups:erasedups
export HISTIGNORE="ls:ll:cd:pwd:exit:date:* --help"
shopt -s histappend
shopt -s cmdhist

# Shell options
shopt -s checkwinsize
shopt -s globstar
shopt -s nocaseglob
shopt -s cdspell
shopt -s dirspell
shopt -s autocd
shopt -s dotglob
shopt -s extglob

# Enable programmable completion features
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash-completion ]; then
    . /usr/share/bash-completion/bash-completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  elif [ -f /opt/homebrew/etc/profile.d/bash_completion.sh ]; then
    . /opt/homebrew/etc/profile.d/bash_completion.sh
  fi
fi

# Source bash functions
if [ -d "$HOME/.config/bash/functions" ]; then
    for func in "$HOME/.config/bash/functions"/*.sh; do
        [ -r "$func" ] && source "$func"
    done
fi

# Prompt configuration (simplified version since bash doesn't support Powerlevel10k)
# You can install starship for a similar experience: https://starship.rs/
if command -v starship &> /dev/null; then
    eval "$(starship init bash)"
else
    # Fallback to a simple but informative prompt with git info
    if [ -f "$HOME/.config/bash/functions/git_prompt.sh" ]; then
        PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\[\033[01;31m\]$(git_prompt_info)\[\033[00m\]\$ '
    else
        PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    fi
fi

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
  elif [[ $(dconf read /org/gnome/desktop/interface/color-scheme 2>/dev/null) == "'prefer-dark'" ]]; then
    theme=$dark_theme
  else
    theme=ansi-light
  fi
  alias cat="COLORTERM=24bit bat --theme=$theme --style=changes,numbers -p"
  export BAT_THEME=$theme
  alias cap='cat -p'
fi

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

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
alias tree='tree -a -C'
alias vim='nvim'
alias s='kitty +kitten ssh'
alias get_idf='. ~/code/embedded/rust-build/export-esp.sh'
alias get_esp32='. ~/.config/bash/export-esp32.sh'
alias get_esprs='. $HOME/export-esp.sh'

alias ggc='git add . && git commit -m.'

# Load direnv
if command -v direnv > /dev/null; then
  eval "$(direnv hook bash)"
fi

# GPG SSH Agent
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# GHCup environment
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"

# FZF
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# ZK Notebook
export ZK_NOTEBOOK_DIR=$HOME/sources/zettels

# Python alias
alias python=python3

# Conditional sourcing for specific environments
if [ "$USERNAME" = "user" ]; then
  [ -f "$HOME/.asdf/asdf.sh" ] && source "$HOME/.asdf/asdf.sh"
fi

# Path additions (these are usually in .bash_profile but included here for compatibility)
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"

# Devcontainer function
devcontainer() {
  if [ "$1" = "exec" ] && [ $# -ge 2 ]; then
    command devcontainer exec --workspace-folder . "$2" "${@:3}"
  else
    command devcontainer "$@" --workspace-folder .
  fi
}

# Bash-specific enhancements

# Better cd command - record directory history
export CDPATH=".:~:~/code:~/projects"

# Colored man pages
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

# Auto-suggestions and syntax highlighting (similar to zsh plugins)
# ble.sh provides both autosuggestions and syntax highlighting
if [ -f "$HOME/.local/share/blesh/ble.sh" ]; then
  source "$HOME/.local/share/blesh/ble.sh"

  # Configure ble.sh settings
  bleopt complete_auto_delay=300
  bleopt highlight_syntax=1
  bleopt highlight_filename=1
  bleopt highlight_variable=1
fi

# Bash-preexec for better command hooks
if [ -f "$HOME/.config/bash/plugins/bash-preexec/bash-preexec.sh" ]; then
  source "$HOME/.config/bash/plugins/bash-preexec/bash-preexec.sh"
elif [ -f "$HOME/.bash-preexec/bash-preexec.sh" ]; then
  source "$HOME/.bash-preexec/bash-preexec.sh"
fi

# Bash-git-prompt for enhanced git integration
if [ -f "$HOME/.config/bash/plugins/bash-git-prompt/gitprompt.sh" ]; then
  GIT_PROMPT_ONLY_IN_REPO=1
  source "$HOME/.config/bash/plugins/bash-git-prompt/gitprompt.sh"
fi

# Sensible bash defaults
if [ -f "$HOME/.config/bash/plugins/bash-sensible/sensible.bash" ]; then
  source "$HOME/.config/bash/plugins/bash-sensible/sensible.bash"
fi

# Source secret environment variables if they exist
[ -f "$HOME/.config/bash/.env.secret" ] && source "$HOME/.config/bash/.env.secret"

# Load local customizations if they exist
[ -f "$HOME/.config/bash/.bashrc.local" ] && source "$HOME/.config/bash/.bashrc.local"

# Terminal title functions (bash equivalent)
update_terminal_title() {
    case "$TERM" in
    xterm*|rxvt*|screen*|tmux*|alacritty|kitty|ghostty)
        echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"
        ;;
    *)
        ;;
    esac
}

# Set PROMPT_COMMAND to update terminal title and other tasks
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }update_terminal_title"

# Save history after each command
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }history -a"

# Improve readline behavior
bind "set completion-ignore-case on"
bind "set show-all-if-ambiguous on"
bind "set mark-symlinked-directories on"
bind "set colored-stats on"
bind "set visible-stats on"
bind "set page-completions off"
bind "set completion-query-items 200"
bind "set completion-prefix-display-length 0"
bind "set input-meta on"
bind "set output-meta on"
bind "set convert-meta off"

# Vi mode (comment out if you prefer emacs mode)
# set -o vi

# Emacs mode (default, like your zsh config)
set -o emacs

# Better history search with arrow keys
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'

# Ctrl+R for reverse history search (enhanced)
bind '"\C-r": reverse-search-history'

# Quick directory navigation
bind '"\e[1;5D": backward-word'
bind '"\e[1;5C": forward-word'
