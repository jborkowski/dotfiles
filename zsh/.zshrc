export SBT_OPTS="-Xmx12G -Xss2M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:ReservedCodeCacheSize=256m -Xverify:none"

export VIMCONFIG=$HOME/.vim
export VIMDATA=$HOME/.vim

# auto java home
JAVA_HOME=$(dirname $( readlink -f $(which java) ))
JAVA_HOME=$(realpath "$JAVA_HOME"/../)
export JAVA_HOME

export PATH=/home/jobo/.local/bin:/usr/local/bin:$PATH
export PATH=$PATH:$VIMCONFIG/pack/minpac/start/fzf/bin

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
export ZSH_CUSTOM=$HOME/.oh-my-zsh-custom

ZSH_THEME="robbyrussell"

[[ -z $ZSH_TMUX_AUTOSTART ]] && ZSH_TMUX_AUTOSTART=true
#ZSH_TMUX_AUTOSTART=true
ZSH_TMUX_AUTOQUIT=false
ZSH_TMUX_AUTOCONNECT=false

#vi-mode vim-interaction
plugins=(git zsh-autosuggestions rbenv gitignore sbt scala mvn cp history rsync vagrant vim-interaction web-search systemd)

source $ZSH/oh-my-zsh.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export VISUAL=nvim
alias vim=nvim
<<<<<<< HEAD
=======
export NIX_IGNORE_SYMLINK_STORE=1
export PATH=$PATH:/nix/var/nix/profiles/default/bin
#. $HOME/.nix-profile/etc/profile.d/nix.sh
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
>>>>>>> 18e7f3f... Emacs Haskel Ide Engine
