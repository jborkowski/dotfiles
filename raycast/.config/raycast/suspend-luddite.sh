#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Luddite: Suspend 
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 💤

ping -c 1 -t 1 luddite > /dev/null 2>&1 && ssh -o ConnectTimeout=5 luddite -- sudo systemctl suspend || echo "Luddite is not available"

