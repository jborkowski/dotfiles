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
export HOMEBREW_NO_ENV_HINTS=y

# Path
export PATH=$HOME/.cargo/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:$HOME/.local/bin:$HOME/.ghcup/bin:$PATH
export PATH=/opt/homebrew/bin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbinu:$PATH
export PATH="$HOME/.emacs.d/.cache/lsp/lua-language-server/bin/":$PATH
export PATH="$HOME/.espressif/tools/xtensa-esp32-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$HOME/.espressif/tools/xtensa-esp32s2-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$HOME/.espressif/tools/xtensa-esp32s3-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$PATH"
export PATH="/Library/TeX/texbin:/Library/TeX/texbin:$HOME/.espressif/tools/xtensa-esp32-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$HOME/.espressif/tools/xtensa-esp32s2-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$HOME/.espressif/tools/xtensa-esp32s3-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:/opt/homebrew/opt/mysql-client/bin:/opt/homebrew/opt/llvm/bin:/opt/homebrew/bin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbinu:$HOME/.cargo/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:$HOME/.local/bin:$HOME/.ghcup/bin:/Applications/kitty.app/Contents/MacOS:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/.cabal/bin:$PATH"
export PATH=$HOME/.orbstack/bin:$PATH
export PATH=$HOME/.config/local/bin:$PATH
export PATH=$HOME/.scripts/:$PATH
export PATH="/opt/homebrew/opt/sphinx-doc/bin:$PATH"

# Source Plugins
source ~/.config/zsh/plugins/powerlevel10k/powerlevel10k.zsh-theme
source ~/.config/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
source ~/.config/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.config/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source ~/.config/zsh/plugins/zsh-edit/zsh-edit.plugin.zsh
source ~/.config/zsh/plugins/zsh-autopair/zsh-autopair.plugin.zsh
source ~/.config/zsh/plugins/zsh-nix-shell/nix-shell.plugin.zsh
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"
# ESP32
export LIBCLANG_PATH="$HOME/.espressif/tools/xtensa-esp32-elf-clang/esp-15.0.0-20221014-aarch64-apple-darwin/esp-clang/lib/"
[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"  || echo  ""


if [ -z "$TERM" ] || [ "$TERM" != "xterm-ghostty" ]; then
    export TERM=xterm-24bit
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export ZK_NOTEBOOK_DIR=$HOME/sources/zettels

if [[ -d "$HOME/.npm-global" ]]; then
  export NPM_CONFIG_PREFIX=$HOME/.npm-global
  export PATH=$HOME/.npm-global/bin:$PATH
fi

. "$HOME/.cargo/env"

export ZAI_API_TOKEN="op://Personal/ZAI_API_TOKEN/password"

