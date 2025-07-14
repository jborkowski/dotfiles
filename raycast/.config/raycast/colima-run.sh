#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Colima Run (on ShieldDrive)
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🤖

if [ ! -d "/Volumes/ShieldDrive" ]; then
    echo "ShieldDrive not mounted. Attempting to mount..."
    
    DISK_ID=$(diskutil list | grep "ShieldDrive" | awk '{print $NF}')
    
    if [ -z "$DISK_ID" ]; then
        echo "ShieldDrive disk not found. Exiting."
        exit 1
    fi
    
    password=$(security find-generic-password -s "shielddrive" -w 2>/dev/null)
    
    if [ -z "$PASSWORD" ]; then
        echo "Password not found in keychain. Please add it with:"
        echo "security add-generic-password -a $USER -s ShieldDrive -w <password>"
        exit 1
    fi
    
    diskutil apfs unlockVolume "$DISK_ID" -passphrase "$PASSWORD"
    
    sleep 3
    
    if [ ! -d "/Volumes/ShieldDrive" ]; then
        echo "Failed to mount ShieldDrive. Exiting."
        exit 1
    fi
    echo "ShieldDrive mounted successfully."
fi

if [ -f "/Volumes/ShieldDrive/run-colima" ]; then
    echo "Running colima script on ShieldDrive..."
    sh /Volumes/ShieldDrive/run-colima
else
    echo "Colima script not found on ShieldDrive."
    exit 1
fi
 
