#!/bin/zsh

ZSH_PLUGINS_DIR=$HOME/.config/zsh/plugins

PLUGINS=(
    'https://github.com/romkatv/powerlevel10k.git'
    'https://github.com/chisui/zsh-nix-shell.git'
    'https://github.com/zsh-users/zsh-history-substring-search.git'
    'https://github.com/zsh-users/zsh-autosuggestions.git'
    'https://github.com/marlonrichert/zsh-edit.git'
    'https://github.com/hlissner/zsh-autopair.git'
    'https://github.com/zdharma-continuum/fast-syntax-highlighting.git'
)


mkdir -p $ZSH_PLUGINS_DIR

for plugin in "${PLUGINS[@]}" ;do
    BASENAME=$(basename $plugin)
    PLUGIN_NAME=${BASENAME%.*}
    PLUGIN_DIR="$ZSH_PLUGINS_DIR/$PLUGIN_NAME"
    if [ ! -d "$PLUGIN_DIR" ]; then
	git clone --depth=1 $plugin $PLUGIN_DIR
    else
	echo "Plugin $PLUGIN_NAME already exists skipping"
    fi
done

## Install fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf

$HOME/.fzf/install --all --no-update-rc
