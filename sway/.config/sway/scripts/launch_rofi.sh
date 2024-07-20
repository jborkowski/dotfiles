#!/bin/bash 

term="alacritty"

rofi_theme="$HOME"/.config/rofi/themes/spotlight.rasi

rofi -terminal "$term" \
  -show combi -combi-modes drun#ssh \
  -display-combi ""  -display-drun ""  \
  -show-icons -modes combi -theme "$rofi_theme"

 
