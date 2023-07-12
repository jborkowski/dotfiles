#!/bin/bash

location="~/Library/Spelling/"
lang=(en_US pl_PL es_ES)

for lang in "${langs[@]}"; do
    country= $(awk -F'[_.]' '{print $1}' <<< "$lang")
    wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/${country}/${lang}.dic -O $location
    wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/${country}/${lang}.aff -O $location
done

curl -L https://gitlab.com/jclosure/dotfiles/-/raw/develop/common/install-terminfo-xterm-24bit.sh | sh
