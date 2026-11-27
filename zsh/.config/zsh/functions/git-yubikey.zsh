# Auto-sign git commits/tags in interactive shells:
# 1. Remote SSH session with a forwarded SSH agent -> sign via forwarded agent key.
# 2. YubiKey plugged into this machine -> sign via YubiKey (libfido2).
# 3. Neither available (or non-interactive) -> commit unsigned.
_yubikey_present() {
  if [[ "$OSTYPE" == darwin* ]]; then
    ioreg -p IOUSB -l -w 0 2>/dev/null | grep -qi "YubiKey"
  elif command -v lsusb >/dev/null 2>&1; then
    lsusb 2>/dev/null | grep -qi "Yubico"
  else
    return 1
  fi
}

git() {
  local arg subcmd="" skip_next=0
  for arg in "$@"; do
    if (( skip_next )); then
      skip_next=0
      continue
    fi
    case "$arg" in
      -C|-c|--git-dir|--work-tree|--namespace|--config-env)
        skip_next=1
        ;;
      -*)
        ;;
      *)
        subcmd="$arg"
        break
        ;;
    esac
  done

  case "$subcmd" in
    commit|c|ci|tag|merge|rebase|revert|cherry-pick)
      if [[ -t 0 ]]; then
        if [[ -n "$SSH_CONNECTION" && -n "$SSH_AUTH_SOCK" ]]; then
          local agent_key
          agent_key=$(ssh-add -L 2>/dev/null | head -n 1)
          if [[ -n "$agent_key" ]]; then
            command git -c commit.gpgsign=true -c tag.gpgsign=true -c "user.signingkey=key::$agent_key" "$@"
            return
          fi
        fi
        if [[ -n "$HOMEBREW_PREFIX" && -x "$HOMEBREW_PREFIX/bin/ssh-keygen" ]] && _yubikey_present; then
          command git -c commit.gpgsign=true -c tag.gpgsign=true "$@"
          return
        fi
      fi
      ;;
  esac

  command git "$@"
}
