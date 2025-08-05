export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$XDG_CONFIG_HOME/local/share"
export XDG_CACHE_HOME="$XDG_CONFIG_HOME/cache"

export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export HISTFILE="$ZDOTDIR/.history"
export HISTSIZE=1000
export SAVEHIST=1000

export ALTERNATE_EDITOR=
export EDITOR="nvim"
export VISUAL="nvim"
export BROWSER="firefox"
export HOMEBREW_NO_ENV_HINTS=y

# Path
export PATH=$HOME/.cargo/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:$HOME/.local/bin:$HOME/.ghcup/bin:$PATH
export PATH=/opt/homebrew/bin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbinu:$PATH

# Source Plugins
source ~/.config/zsh/plugins/powerlevel10k/powerlevel10k.zsh-theme
source ~/.config/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
source ~/.config/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.config/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source ~/.config/zsh/plugins/zsh-edit/zsh-edit.plugin.zsh
source ~/.config/zsh/plugins/zsh-autopair/zsh-autopair.plugin.zsh
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"

export PATH="/Library/TeX/texbin:/Library/TeX/texbin:/opt/homebrew/opt/mysql-client/bin:/opt/homebrew/opt/llvm/bin:/opt/homebrew/bin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbinu:$HOME/.cargo/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:$HOME/.local/bin:$HOME/.ghcup/bin:/Applications/kitty.app/Contents/MacOS:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/.cabal/bin"

. "$HOME/.cargo/env"

if [ -z "$TERM" ] || [ "$TERM" != "xterm-ghostty" ] || [ -z "$(infocmp xterm-ghostty 2>/dev/null)" ]; then
    export TERM=xterm-24bit
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# export LIBGL_ALWAYS_SOFTWARE=1

[ -f "$HOME/.config/zsh/.env.secret" ] && source "$HOME/.config/zsh/.env.secret"  || echo  ""
