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
export PATH="$HOME/.emacs.d/.cache/lsp/lua-language-server/bin/":$PATH
export PATH="$HOME/.espressif/tools/xtensa-esp32-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$HOME/.espressif/tools/xtensa-esp32s2-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$HOME/.espressif/tools/xtensa-esp32s3-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$PATH"
export PATH="/Library/TeX/texbin:/Library/TeX/texbin:$HOME/.espressif/tools/xtensa-esp32-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$HOME/.espressif/tools/xtensa-esp32s2-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:$HOME/.espressif/tools/xtensa-esp32s3-elf-gcc/8_4_0-esp-2021r2-patch3-aarch64-apple-darwin/bin/:/opt/homebrew/opt/mysql-client/bin:/opt/homebrew/opt/llvm/bin:/opt/homebrew/bin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbinu:$HOME/.cargo/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:$HOME/.local/bin:$HOME/.ghcup/bin:/Applications/kitty.app/Contents/MacOS:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/.cabal/bin:$PATH"
export PATH=$HOME/.roswell/bin:$PATH

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
. "$HOME/.cargo/env"
export GH_TOKEN="op://Personal/GitHub Jonatan/Section_2CBCCC6A7E4040BC9B694F19FD61BF56/personal_access_token"

. "$HOME/.cargo/env"

if [ -z "$TERM" ] || [ "$TERM" != "xterm-ghostty" ]; then
    export TERM=xterm-24bit
fi

if [ -e "$MOME/.nix-profile/etc/profile.d/nix.sh" ]; then . "$HOME/.nix-profile/etc/profile.d/nix.sh"; fi # added by Nix installer

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# export LIBGL_ALWAYS_SOFTWARE=1

# opam configuration
[[ ! -r "$MOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh"  > /dev/null 2> /dev/null

export PATH="/opt/homebrew/opt/sphinx-doc/bin:$PATH"
export ZK_NOTEBOOK_DIR=$HOME/sources/zettels

[ -f "$HOME/.config/zsh/.env.secret" ] && source "$HOME/.config/zsh/.env.secret" > /dev/null 2> /dev/null
