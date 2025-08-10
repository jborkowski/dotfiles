#!/bin/bash

set -euo pipefail

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Luddite Power Button
# @raycast.mode silent
# @raycast.icon 🤖

KVMHOST="ludditekvm"
POWER_PIN="503"
RESET_PIN="505"
GPIO_PATH="/sys/class/gpio/gpio${POWER_PI}"

if ssh "root@${KVMHOST}" -q -o ConnectTimeout=1 -o BatchMode=yes <<EOF
g
  echo 1 > "${GPIO_PATH}/value"
  sleep 1
  echo 0 > "${GPIO_PATH}/value"
EOF
then
  echo "Done"
else
  echo "Failed to connect to ${KVMHOST}" >&2
  exit 1
fi
