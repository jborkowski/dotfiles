#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Colima Stop (with unmount ShieldDrive)
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🛑

colima stop && (diskutil unmount /Volumes/ShieldDrive || diskutil unmountDisk force /Volumes/ShieldDrive || (lsof /Volumes/ShieldDrive | awk 'NR>1 {print $2}' | sort -u | xargs -r kill -9 && diskutil unmount force /Volumes/ShieldDrive))


