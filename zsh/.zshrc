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
export ZSH="/home/jobo/.oh-my-zsh"
export ZSH_CUSTOM="/home/jobo/.oh-my-zsh-custom"

ZSH_THEME="robbyrussell"

[[ -z $ZSH_TMUX_AUTOSTART ]] && ZSH_TMUX_AUTOSTART=true
#ZSH_TMUX_AUTOSTART=true
ZSH_TMUX_AUTOQUIT=false
ZSH_TMUX_AUTOCONNECT=false

#vi-mode vim-interaction
plugins=(emacs git zsh-autosuggestions rbenv gitignore sbt scala mvn cp history rsync tmux vagrant vim-interaction web-search systemd)

source $ZSH/oh-my-zsh.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export VISUAL=nvim
alias vim=nvim
