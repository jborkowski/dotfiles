#!/bin/bash

if command -v dnf &>/dev/null; then
  sudo dnf install gtk-murrine-engine wmenu -y
else
  echo "not supported"
fi

## clone and install theme from git@github.com:Fausto-Korpsvart/Nightfox-GTK-Theme.git if not already installde (cloned to ~/sources/)
THEME_DIR="$HOME/sources/Nightfox-GTK-Theme"
if [ ! -d "$THEME_DIR" ]; then
  echo "Cloning Nightfox GTK Theme..."
  mkdir -p "$HOME/sources"
  git clone git@github.com:Fausto-Korpsvart/Nightfox-GTK-Theme.git "$THEME_DIR"
fi

# Install the theme
echo "Installing Nightfox GTK Theme..."
mkdir -p "$HOME/.themes"

# Install the icons if available
if [ -d "$THEME_DIR/icons" ]; then
  echo "Installing Nightfox icons..."
  mkdir -p "$HOME/.icons"
  cp -r "$THEME_DIR/icons/"* "$HOME/.icons/"
  (
    cd "$THEME_DIR/themes"
    ./install.sh -n Dayfox -t default -l -c light
    ./install.sh -n Duskfox -t default -l -c dark
  )
fi
