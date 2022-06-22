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
alias cat='bat -p'
alias cp='xcp'
alias e='emacsclient -t'
alias find='fd'
alias ll='exa -lF --color-scale --no-user --no-time --no-permissions --group-directories-first --icons -a'
alias ls='exa -lF --group-directories-first --icons -a'
alias ps='ps'
alias top='btm'
alias tree='tree -a -C'
alias vim='nvim'
alias s='kitty +kitten ssh'
alias get_idf='. ~/code/esp-idf/export.sh'
alias get_esp32='. ~/.config/zsh/export-esp32.sh'

# Load
autoload -U compinit; compinit

eval "$(direnv hook zsh)"

export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

if [ -x "$(command -v fzf)" ]; then
    source /etc/zsh_completion.d/fzf-key-bindings
fi

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh
