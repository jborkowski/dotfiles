#!/bin/bash -
#===============================================================================
#
#          FILE: restow.sh
#
#         USAGE: ./restow.sh
#
#   DESCRIPTION: initialize dotfiles symlinks using GNU Stow
#
#  REQUIREMENTS: stow, neovim
#        AUTHOR: JONATAN BORKOWSKI (joantan.borkowski@pm.me),
#       CREATED: 17/11/19 12:07
#      REVISION: 0.0.1
#===============================================================================

set -o nounset                              # Treat unset variables as an error

conf_file=~/.dotfiles
default_profile=home

profile() {
  local profile
  if [ -e $conf_file ]; then
    profile=$(cat $conf_file | grep -v '^#' | grep '^profile=' | cut -d '=' -f 2)
  fi
  [[ -z $profile ]] && echo $default_profile || echo $profile
}

profile=$(profile)

echo "Active profile : $profile"

stow --override=* -R -v -t ~/ -d git $profile
stow -R -v -t ~/ my_emacs_d
