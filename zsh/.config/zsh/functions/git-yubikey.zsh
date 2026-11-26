# Auto-sign git commits/tags with YubiKey when plugged in (interactive shells only).
# Falls back to unsigned commits when unplugged or non-interactive.
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
      if [[ -t 0 && -x /opt/jbrew/bin/ssh-keygen ]] && _yubikey_present; then
        command git -c commit.gpgsign=true -c tag.gpgsign=true "$@"
        return
      fi
      ;;
  esac

  command git "$@"
}
