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
export PATH="/Library/TeX/texbin:$PATH"                                             
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.ghcup/bin:$PATH"
export PATH="/Applications/kitty.app/Contents/MacOS:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"

if BUN_GLOBAL_BIN=$(bun pm bin -g 2>/dev/null) && [ -d "$BUN_GLOBAL_BIN" ]; then                                        
  export PATH="$HOME/.bun/bin:$BUN_GLOBAL_BIN:$PATH"                                                                    
else                                                                                                                    
  export PATH="$HOME/.bun/bin:$PATH"                                                                                    
fi 

# nebius-claude — ~/sources checkout on PATH; best-effort clone if missing
() {
  local dir="${HOME}/sources/nebius-claude"
  local bin="${dir}/nebius-claude"
  if [[ ! -x "$bin" && ! -d "${dir}/.git" ]] && (( $+commands[git] )); then
    mkdir -p "${HOME}/sources"
    git clone --quiet --depth=1 git@github.com:jborkowski/nebius-claude.git "$dir" 2>/dev/null || true
  fi
  [[ -x "$bin" ]] && path=("$dir" $path)
}

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

if [[ -d "$HOME/.npm-global" ]]; then
  export NPM_CONFIG_PREFIX=$HOME/.npm-global
  export PATH=$HOME/.npm-global/bin:$PATH
fi

if [[ "$(uname)" == "Darwin" ]]; then
  alias bearcli='/Applications/Bear.app/Contents/MacOS/bearcli'
  export LLM_WIKI_ROOT="$HOME/Documents/llm-wiki"
  export SSH_SK_PROVIDER=/usr/lib/ssh-keychain.dylib
fi

export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"
