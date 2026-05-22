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
export PATH="$(bun pm bin -g):$PATH"



# ESP32
export LIBCLANG_PATH="$HOME/.espressif/tools/xtensa-esp32-elf-clang/esp-15.0.0-20221014-aarch64-apple-darwin/esp-clang/lib/"
[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"  || echo  ""


# Keep TERM as-is inside tmux/screen and under Ghostty; otherwise fall back to
# the custom xterm-24bit terminfo. Overriding tmux-256color → xterm-24bit makes
# nvim send DA/XTVERSION queries that leak Ghostty's DCS reply onto the screen.
case "$TERM" in
    xterm-ghostty|tmux*|screen*) ;;
    *) export TERM=xterm-24bit ;;
esac

export ZK_NOTEBOOK_DIR=$HOME/sources/zettels

if [[ -d "$HOME/.npm-global" ]]; then
  export NPM_CONFIG_PREFIX=$HOME/.npm-global
  export PATH=$HOME/.npm-global/bin:$PATH
fi

if [[ "$(uname)" == "Darwin" ]]; then
  alias bearcli='/Applications/Bear.app/Contents/MacOS/bearcli'
fi
