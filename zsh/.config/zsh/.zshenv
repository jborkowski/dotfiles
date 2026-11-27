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

# Deduplicate PATH/FPATH/MANPATH/INFOPATH across nested shells
typeset -U path fpath manpath infopath

# Detect the active Homebrew installation for this machine/user.
# Prefers a brew owned by the current user (e.g. /opt/jbrew on a shared Mac),
# then falls back to the first executable brew found. Any other installed brew
# prefixes are kept on PATH behind the active prefix as read-only fallbacks.
_init_homebrew() {
  local -a candidates=(
    /opt/jbrew
    /opt/homebrew
    /usr/local
    /home/linuxbrew/.linuxbrew
    "$HOME/.linuxbrew"
  )
  local prefix="" p opt_pkg
  _homebrew_fallbacks=()

  for p in "${candidates[@]}"; do
    if [[ -x "$p/bin/brew" && -O "$p/bin/brew" ]]; then
      prefix="$p"
      break
    fi
  done

  if [[ -z "$prefix" ]]; then
    for p in "${candidates[@]}"; do
      if [[ -x "$p/bin/brew" ]]; then
        prefix="$p"
        break
      fi
    done
  fi

  [[ -z "$prefix" ]] && return 0

  for p in "${candidates[@]}"; do
    if [[ "$p" != "$prefix" && -x "$p/bin/brew" ]]; then
      _homebrew_fallbacks+=("$p")
    fi
  done

  # Secondary/shared brew prefixes first so active brew always takes precedence
  for p in "${(@Oa)_homebrew_fallbacks}"; do
    for opt_pkg in mysql-client llvm; do
      [[ -d "$p/opt/$opt_pkg/bin" ]] && path=("$p/opt/$opt_pkg/bin" $path)
    done
    [[ -d "$p/sbin" ]] && path=("$p/sbin" $path)
    [[ -d "$p/bin" ]] && path=("$p/bin" $path)
  done

  # Initialize active brew (skip forking brew if already initialized for this prefix)
  if [[ "$HOMEBREW_PREFIX" != "$prefix" || -z "$HOMEBREW_CELLAR" ]]; then
    eval "$("$prefix/bin/brew" shellenv)"
  else
    [[ -d "$prefix/sbin" ]] && path=("$prefix/sbin" $path)
    [[ -d "$prefix/bin" ]] && path=("$prefix/bin" $path)
    [[ -d "$prefix/share/zsh/site-functions" ]] && fpath=("$prefix/share/zsh/site-functions" $fpath)
  fi

  # Keg-only formulae in active brew
  for opt_pkg in mysql-client llvm; do
    [[ -d "$prefix/opt/$opt_pkg/bin" ]] && path=("$prefix/opt/$opt_pkg/bin" $path)
  done

  # For user-isolated macOS installs (e.g. /opt/jbrew), put casks in ~/Applications
  if [[ "$OSTYPE" == darwin* && "$prefix" != /opt/homebrew && "$prefix" != /usr/local ]]; then
    export HOMEBREW_CASK_OPTS="${HOMEBREW_CASK_OPTS:---appdir=$HOME/Applications}"
  fi
}

# Build PATH in priority order (re-invoked by .zprofile after macOS path_helper)
_init_paths() {
  [[ -d /Library/TeX/texbin ]] && path=(/Library/TeX/texbin $path)
  [[ -d /usr/local/bin ]] && path=(/usr/local/bin $path)

  _init_homebrew

  path=(
    "$HOME/.cabal/bin"
    "$HOME/.ghcup/bin"
    "$HOME/.local/bin"
    "$HOME/.cargo/bin"
    $path
  )
  [[ -d /Applications/kitty.app/Contents/MacOS ]] && path=(/Applications/kitty.app/Contents/MacOS $path)
  [[ -n "$_BUN_GLOBAL_BIN" && -d "$_BUN_GLOBAL_BIN" ]] && path=("$_BUN_GLOBAL_BIN" $path)
  path=("$HOME/.bun/bin" $path)
  [[ -x "$HOME/sources/nebius-claude/nebius-claude" ]] && path=("$HOME/sources/nebius-claude" $path)
  if [[ -d "$HOME/.npm-global" ]]; then
    export NPM_CONFIG_PREFIX="$HOME/.npm-global"
    path=("$HOME/.npm-global/bin" $path)
  fi
}

_init_paths

if [[ -z "$_BUN_GLOBAL_BIN" ]] && _BUN_GLOBAL_BIN=$(bun pm bin -g 2>/dev/null) && [[ -d "$_BUN_GLOBAL_BIN" ]]; then
  path=("$HOME/.bun/bin" "$_BUN_GLOBAL_BIN" $path)
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

if [[ "$(uname)" == "Darwin" ]]; then
  alias bearcli='/Applications/Bear.app/Contents/MacOS/bearcli'
  export LLM_WIKI_ROOT="$HOME/Documents/llm-wiki"
  if [[ -n "$HOMEBREW_PREFIX" && -x "$HOMEBREW_PREFIX/bin/ssh" ]]; then
    unset SSH_SK_PROVIDER
  else
    export SSH_SK_PROVIDER=/usr/lib/ssh-keychain.dylib
  fi
fi

export NVM_DIR="$HOME/.nvm"
() {
  local p
  for p in "$HOMEBREW_PREFIX" "${_homebrew_fallbacks[@]}"; do
    [[ -n "$p" && -s "$p/opt/nvm/nvm.sh" ]] || continue
    \. "$p/opt/nvm/nvm.sh"
    [[ -s "$p/opt/nvm/etc/bash_completion.d/nvm" ]] && \. "$p/opt/nvm/etc/bash_completion.d/nvm"
    break
  done
}
